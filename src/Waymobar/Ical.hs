{-# LANGUAGE OverloadedStrings #-}

-- | A small iCalendar (RFC 5545) reader: just what is needed to list events.
module Waymobar.Ical
    ( RawEvent (..)
    , Stamp (..)
    , parseIcal
    , wallClock
    , occurrences
    ) where

import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time

-- | A point in time as written in the file.
data Stamp
    = -- | An all-day date.
      DateOnly Day
    | Utc UTCTime
    | -- | Local time wherever the reader is.
      Floating LocalTime
    | -- | Local time in the named time zone.
      Zoned Text LocalTime

data RawEvent = RawEvent
    { reUid :: Text
    , reSummary :: Text
    , reStart :: Stamp
    , reEnd :: Maybe Stamp
    , reDuration :: Maybe NominalDiffTime
    , reRule :: Maybe Rule
    , reExDates :: [LocalTime]
    -- ^ Occurrences removed from the series, as wall-clock times.
    , reRecurrenceId :: Maybe LocalTime
    -- ^ Set on an event that replaces one occurrence of a series.
    , reCancelled :: Bool
    }

data Freq = Daily | Weekly | Monthly | Yearly

data Rule = Rule
    { ruFreq :: Freq
    , ruInterval :: Integer
    , ruCount :: Maybe Int
    , ruUntil :: Maybe Day
    , ruByDay :: [(Maybe Int, DayOfWeek)]
    -- ^ Week days, with an ordinal for "the 2nd Tuesday" (negative = from the end).
    }

-- | The date and time as written, without converting between zones.
wallClock :: Stamp -> LocalTime
wallClock s = case s of
    DateOnly d -> LocalTime d midnight
    Utc t -> utcToLocalTime utc t
    Floating t -> t
    Zoned _ t -> t

-- Parsing -------------------------------------------------------------------

data Prop = Prop
    { prName :: Text
    , prParams :: [(Text, Text)]
    , prValue :: Text
    }

-- | Every VEVENT in the text; events that cannot be understood are skipped.
parseIcal :: Text -> [RawEvent]
parseIcal = mapMaybe event . blocks . map contentLine . unfold

-- Long lines are folded: a line starting with a space continues the previous one.
unfold :: Text -> [Text]
unfold = go . T.lines . T.replace "\r\n" "\n"
  where
    go (l : rest)
        | (cont, others) <- span continuation rest = T.concat (l : map (T.drop 1) cont) : go others
    go [] = []
    continuation l = " " `T.isPrefixOf` l || "\t" `T.isPrefixOf` l

-- NAME;PARAM=VALUE;...:value — a parameter value may be quoted and contain ':'.
contentLine :: Text -> Prop
contentLine l = Prop (T.toUpper name) params (T.drop 1 value)
  where
    (before, value) = breakUnquoted ':' l
    (name, rest) = T.break (== ';') before
    params =
        [ (T.toUpper k, T.dropAround (== '"') (T.drop 1 v))
        | p <- splitUnquoted ';' (T.drop 1 rest)
        , let (k, v) = T.break (== '=') p
        ]

breakUnquoted :: Char -> Text -> (Text, Text)
breakUnquoted c t = T.splitAt (go False 0 (T.unpack t)) t
  where
    go :: Bool -> Int -> String -> Int
    go _ n [] = n
    go quoted n (x : xs)
        | x == '"' = go (not quoted) (n + 1) xs
        | x == c && not quoted = n
        | otherwise = go quoted (n + 1) xs

splitUnquoted :: Char -> Text -> [Text]
splitUnquoted c t
    | T.null t = []
    | otherwise = case breakUnquoted c t of
        (a, b)
            | T.null b -> [a]
            | otherwise -> a : splitUnquoted c (T.drop 1 b)

-- The properties of each VEVENT, leaving out nested components (alarms).
blocks :: [Prop] -> [[Prop]]
blocks ps = case dropWhile (not . begins "VEVENT") ps of
    [] -> []
    (_ : rest) ->
        let (body, after) = break (ends "VEVENT") rest
         in topLevel body : blocks (drop 1 after)
  where
    begins what p = prName p == "BEGIN" && T.toUpper (prValue p) == what
    ends what p = prName p == "END" && T.toUpper (prValue p) == what
    topLevel = go (0 :: Int)
      where
        go _ [] = []
        go depth (p : rest)
            | prName p == "BEGIN" = go (depth + 1) rest
            | prName p == "END" = go (max 0 (depth - 1)) rest
            | depth == 0 = p : go depth rest
            | otherwise = go depth rest

event :: [Prop] -> Maybe RawEvent
event ps = do
    start <- prop "DTSTART" >>= stamp
    pure
        RawEvent
            { reUid = maybe "" prValue (prop "UID")
            , reSummary = maybe "" (unescape . prValue) (prop "SUMMARY")
            , reStart = start
            , reEnd = prop "DTEND" >>= stamp
            , reDuration = prop "DURATION" >>= duration . prValue
            , reRule = prop "RRULE" >>= rule . prValue
            , reExDates =
                [ wallClock s
                | p <- ps
                , prName p == "EXDATE"
                , v <- T.splitOn "," (prValue p)
                , Just s <- [stamp p{prValue = v}]
                ]
            , reRecurrenceId = wallClock <$> (prop "RECURRENCE-ID" >>= stamp)
            , reCancelled = fmap (T.toUpper . prValue) (prop "STATUS") == Just "CANCELLED"
            }
  where
    prop name = listToMaybe [p | p <- ps, prName p == name]

-- 20261114, 20261114T100000 or 20261114T100000Z, maybe with a TZID parameter.
stamp :: Prop -> Maybe Stamp
stamp p
    | T.length v == 8 = DateOnly <$> parse "%Y%m%d" v
    | "Z" `T.isSuffixOf` v = Utc . localTimeToUTC utc <$> parse "%Y%m%dT%H%M%S" (T.dropEnd 1 v)
    | Just zone <- lookup "TZID" (prParams p) = Zoned zone <$> parse "%Y%m%dT%H%M%S" v
    | otherwise = Floating <$> parse "%Y%m%dT%H%M%S" v
  where
    v = T.strip (prValue p)
    parse :: (ParseTime t) => String -> Text -> Maybe t
    parse format = parseTimeM False defaultTimeLocale format . T.unpack

-- P1D, PT1H30M, P1W...
duration :: Text -> Maybe NominalDiffTime
duration t = case T.unpack (T.toUpper (T.strip t)) of
    'P' : rest -> Just (go False rest)
    _ -> Nothing
  where
    go :: Bool -> String -> NominalDiffTime
    go _ ('T' : rest) = go True rest
    go inTime s = case span isDigit s of
        (digits@(_ : _), unit : rest) -> fromInteger (read digits) * seconds inTime unit + go inTime rest
        _ -> 0
    seconds inTime unit = case unit of
        'W' -> 7 * 86400
        'D' -> 86400
        'H' -> 3600
        'M' | inTime -> 60
        'S' -> 1
        _ -> 0

rule :: Text -> Maybe Rule
rule t = do
    freq <-
        field "FREQ" >>= \f -> case f of
            "DAILY" -> Just Daily
            "WEEKLY" -> Just Weekly
            "MONTHLY" -> Just Monthly
            "YEARLY" -> Just Yearly
            _ -> Nothing
    pure
        Rule
            { ruFreq = freq
            , ruInterval = max 1 (fromMaybe 1 (field "INTERVAL" >>= number))
            , ruCount = fromInteger <$> (field "COUNT" >>= number)
            , ruUntil = field "UNTIL" >>= parseTimeM False defaultTimeLocale "%Y%m%d" . take 8 . T.unpack
            , ruByDay = maybe [] (mapMaybe byDay . T.splitOn ",") (field "BYDAY")
            }
  where
    fields = [(T.toUpper k, T.toUpper (T.drop 1 v)) | p <- T.splitOn ";" t, let (k, v) = T.break (== '=') p]
    field name = lookup name fields
    number :: Text -> Maybe Integer
    number v = case reads (T.unpack v) of
        [(n, "")] -> Just n
        _ -> Nothing
    byDay v = do
        let (ordinal, code) = T.splitAt (T.length v - 2) v
        day <- lookup code (zip ["MO", "TU", "WE", "TH", "FR", "SA", "SU"] [Monday ..])
        if T.null ordinal
            then Just (Nothing, day)
            else (\n -> (Just (fromInteger n), day)) <$> number (T.dropWhile (== '+') ordinal)

-- "\n" becomes a space: summaries are shown on one line.
unescape :: Text -> Text
unescape = T.pack . go . T.unpack
  where
    go ('\\' : c : rest)
        | c `elem` ("nN" :: String) = ' ' : go rest
        | otherwise = c : go rest
    go (c : rest) = c : go rest
    go [] = []

-- Recurrence ----------------------------------------------------------------

{- | Wall-clock start times of an event up to (and including) the given day:
just its own start if it does not repeat, otherwise the occurrences of its
rule, minus the excluded dates. Rules with parts this reader does not know
(BYMONTHDAY, BYSETPOS...) are followed as if those parts were not there.
-}
occurrences :: Day -> RawEvent -> [LocalTime]
occurrences lastDay ev = case reRule ev of
    Nothing -> [start]
    Just r ->
        filter (`notElem` reExDates ev)
            . takeWhile ((<= lastDay) . localDay)
            . maybe id (\u -> takeWhile ((<= u) . localDay)) (ruUntil r)
            . maybe id take (ruCount r)
            . take 50000
            . map (\d -> LocalTime d (localTimeOfDay start))
            $ days r
  where
    start = wallClock (reStart ev)
    day0 = localDay start
    (y0, m0, d0) = toGregorian day0
    steps r = [0, ruInterval r ..]
    days r = case ruFreq r of
        Daily -> [addDays n day0 | n <- steps r]
        Weekly ->
            let monday = addDays (1 - toInteger (fromEnum (dayOfWeek day0))) day0
                offsets = sort [toInteger (fromEnum wd) - 1 | (_, wd) <- ruByDay r]
                inWeek = if null offsets then [diffDays day0 monday] else offsets
             in dropWhile (< day0) [addDays (7 * n + o) monday | n <- steps r, o <- inWeek]
        Monthly ->
            dropWhile (< day0) . concat $
                [ case ruByDay r of
                    [] -> catMaybes [fromGregorianValid y m d0]
                    wds -> sort (catMaybes [nthWeekday y m nth wd | (Just nth, wd) <- wds])
                | n <- steps r
                , let (y, m) = monthAfter n
                ]
        Yearly -> catMaybes [fromGregorianValid (y0 + n) m0 d0 | n <- steps r]
    monthAfter n =
        let total = toInteger (m0 - 1) + n
         in (y0 + total `div` 12, fromInteger (total `mod` 12) + 1)

-- | The nth given week day of a month; negative counts from the end.
nthWeekday :: Integer -> Int -> Int -> DayOfWeek -> Maybe Day
nthWeekday y m n wd
    | n > 0 = pick (n - 1) matching
    | n < 0 = pick (negate n - 1) (reverse matching)
    | otherwise = Nothing
  where
    matching = [d | d <- map (fromGregorian y m) [1 .. gregorianMonthLength y m], dayOfWeek d == wd]
    pick i xs = listToMaybe (drop i xs)
