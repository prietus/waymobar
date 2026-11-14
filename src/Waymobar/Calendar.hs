{-# LANGUAGE OverloadedStrings #-}
-- | The agenda: upcoming events from CalDAV accounts (iCloud, Google, any
-- server) and iCal addresses, for a popover under the clock or a widget
-- of its own. Fetched with @curl@, so no TLS library is needed.
module Waymobar.Calendar
  ( CalendarConfig (..)
  , Account (..)
  , Secret (..)
  , defaultCalendar
  , Agenda (..)
  , Event (..)
  , calendar
  , withCalendar
  , agendaMarkup
  , agendaToday
    -- * Lower level
  , fetchAgenda
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forM, forever, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.Either (lefts, rights)
import Data.Int (Int64)
import Data.IORef
import Data.List (intercalate, nub, sortOn)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.Directory (getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, hPutStrLn, stderr)
import System.Process

import Waymobar.Core
import Waymobar.Ical
import Waymobar.Widgets (escape, shorten)
import Waymobar.XML

-- | Where a password comes from. An account wants an app password (an
-- "app-specific password" for iCloud), not the one of the account itself.
data Secret
  = Password String          -- ^ written in the configuration
  | PasswordFile FilePath    -- ^ the first line of a file (@~/@ is expanded); keep it readable only by you
  | PasswordCommand String   -- ^ the first line printed by a shell command, e.g. @"secret-tool lookup service icloud"@
  deriving (Eq, Show)

-- | A source of events.
data Account
  = ICloud String Secret          -- ^ every calendar of an iCloud account: Apple ID (the mail address) and an app-specific password
  | Google String Secret          -- ^ the main calendar of a Google account: mail address and an app password
  | CalDav String String Secret   -- ^ every calendar of an account on a CalDAV server (Nextcloud, Fastmail, Radicale...): URL, user, password
  | IcsUrl String                 -- ^ a read-only iCal address (@https://@ or @webcal://@)
  deriving (Eq, Show)

data CalendarConfig = CalendarConfig
  { calendarAccounts :: [Account]
  , calendarDays     :: Int      -- ^ days ahead that are fetched
  , calendarInterval :: Double   -- ^ seconds between updates
  , calendarClickRefreshes :: Bool  -- ^ a left click fetches the events now
  , calendarHover    :: Agenda -> String
    -- ^ markup of the popover shown while the pointer is on the widget
    -- (@""@ = none)
  , calendarMarkup   :: Agenda -> String -> String
    -- ^ with 'withCalendar': rewrites the markup of the wrapped widget,
    -- e.g. to mark in the bar that there are events today
  }

-- | No accounts, two weeks ahead, every ten minutes, 'agendaMarkup' in
-- the popover, the wrapped widget as it is.
defaultCalendar :: CalendarConfig
defaultCalendar = CalendarConfig [] 14 600 True agendaMarkup (const id)

data Event = Event
  { eventStart  :: LocalTime
  , eventEnd    :: LocalTime     -- ^ both in the system's time zone
  , eventAllDay :: Bool
  , eventTitle  :: String
  , eventColor  :: Maybe String  -- ^ the calendar's own colour (@#rrggbb@), when the server gives one
  } deriving (Eq, Show)

-- | What is known when the bar is drawn.
data Agenda = Agenda
  { agendaNow    :: LocalTime
  , agendaEvents :: [Event]    -- ^ the ones that are not over, soonest first
  , agendaErrors :: [String]   -- ^ accounts that could not be read
  , agendaLoaded :: Bool       -- ^ False until the first answer
  } deriving (Eq, Show)

-- | The events of today (those that started earlier and go on included).
agendaToday :: Agenda -> [Event]
agendaToday a = [ e | e <- agendaEvents a, localDay (eventStart e) <= localDay (agendaNow a) ]

------------------------------------------------------------------------------
-- Widgets

-- | Adds the agenda to another widget, usually the clock: its popover
-- shows 'calendarHover' while the pointer is on it, and 'calendarMarkup'
-- can change what the bar shows after the agenda.
--
-- > withCalendar defaultCalendar
-- >   { calendarAccounts = [ICloud "me@icloud.com" (PasswordFile "~/.config/waymobar/icloud-password")] }
-- >   (clock "%a %d %b  %H:%M")
withCalendar :: CalendarConfig -> Widget -> Widget
withCalendar cfg (Widget inner) = Widget $ \env -> do
  (agenda, click) <- startAgenda cfg env
  draw <- inner env
  pure $ \out -> do
    a <- agenda
    bs <- draw out
    let h = calendarHover cfg a
        both b c = maybe (pure ()) ($ c) (blockOnClick b) >> click c
    pure [ b { blockMarkup = calendarMarkup cfg a (blockMarkup b)
             , blockHoverMarkup = if null h then blockHoverMarkup b else Just h
             , blockOnClick = Just (both b) }
         | b <- bs ]

-- | A widget of its own: @f@ gives the markup of the bar (@""@ hides it).
--
-- > calendar cfg $ \a -> case agendaEvents a of
-- >   e : _ -> escape (eventTitle e)
-- >   [] -> ""
calendar :: CalendarConfig -> (Agenda -> String) -> Widget
calendar cfg fmt = Widget $ \env -> do
  (agenda, click) <- startAgenda cfg env
  pure $ \_ -> do
    a <- agenda
    let s = fmt a
        h = calendarHover cfg a
    pure [ (block s) { blockOnClick = Just click, blockHoverMarkup = if null h then Nothing else Just h }
         | not (null s) ]

data State = State
  { stEvents :: [Event]
  , stErrors :: [String]
  , stLoaded :: Bool
  }

-- | Starts fetching; gives how to read the agenda and the click handler.
startAgenda :: CalendarConfig -> BarEnv -> IO (IO Agenda, Click -> IO ())
startAgenda cfg env = do
  ref <- newIORef (State [] [] False)
  kick <- newTVarIO False
  let -- wait for the timer, or for a click
      wait :: Double -> IO ()
      wait secs = do
        d <- registerDelay (round (secs * 1000000))
        atomically $ (readTVar d >>= check) `orElse` (readTVar kick >>= check >> writeTVar kick False)
  void $ forkIO $ forever $ do
    old <- readIORef ref
    r <- E.try (refresh cfg old)
    let new = case r of
          Right st -> st
          Left e -> old { stErrors = [show (e :: E.SomeException)], stLoaded = True }
    writeIORef ref new
    redraw env
    -- try again sooner after a failure
    wait (if null (stErrors new) then calendarInterval cfg else min 120 (calendarInterval cfg))
  -- events that are over drop off, and "today" moves on
  void $ forkIO $ forever $ threadDelay 60000000 >> redraw env
  let agenda = currentAgenda <$> readIORef ref <*> localNow
      click c = when (calendarClickRefreshes cfg && clickButton c == LeftButton) $
        atomically (writeTVar kick True)
  pure (agenda, click)

localNow :: IO LocalTime
localNow = zonedTimeToLocalTime <$> getZonedTime

currentAgenda :: State -> LocalTime -> Agenda
currentAgenda st now = Agenda
  { agendaNow = now
  , agendaEvents = sortOn eventStart
      [ e | e <- stEvents st, eventEnd e > now || (eventEnd e == eventStart e && eventStart e >= now) ]
  , agendaErrors = stErrors st
  , agendaLoaded = stLoaded st
  }

-- | Fetch the events of every account now.
fetchAgenda :: CalendarConfig -> IO Agenda
fetchAgenda cfg = currentAgenda <$> refresh cfg (State [] [] False) <*> localNow

refresh :: CalendarConfig -> State -> IO State
refresh cfg previous = do
  today <- localDay <$> localNow
  results <- forM (calendarAccounts cfg) $ \account -> do
    r <- fetchAccount (fromIntegral (max 1 (calendarDays cfg))) today account
    pure (either (\err -> Left (accountLabel account ++ ": " ++ T.unpack err)) Right r)
  let errors = lefts results
  mapM_ (\err -> hPutStrLn stderr ("waymobar: calendar: " ++ err)) errors
  pure State
    { -- if nothing could be fetched, what was there before is better than nothing
      stEvents = if null (rights results) then stEvents previous else concat (rights results)
    , stErrors = if null (calendarAccounts cfg) then ["No calendar accounts in the configuration"] else errors
    , stLoaded = True
    }

-- | How an account is named in error messages; never the password.
accountLabel :: Account -> String
accountLabel = \case
  ICloud user _ -> "iCloud · " ++ user
  Google user _ -> "Google · " ++ user
  CalDav url user _ -> "CalDAV · " ++ (if null user then url else user)
  IcsUrl _ -> "iCal address"

-- | The password itself.
readSecret :: Secret -> IO (Either Text Text)
readSecret = \case
  Password p -> pure (Right (T.pack p))
  PasswordFile path -> do
    file <- expandHome path
    r <- E.try (B.readFile file)
    pure $ case r of
      Right bytes -> nonEmpty ("the password file " <> T.pack path <> " is empty") (decode bytes)
      Left e -> Left ("cannot read the password: " <> T.pack (show (e :: E.IOException)))
  PasswordCommand cmd -> do
    r <- E.try (readCreateProcessWithExitCode (shell cmd) "")
    pure $ case r of
      Right (ExitSuccess, out, _) -> nonEmpty "the password command printed nothing" (T.pack out)
      Right (ExitFailure n, _, _) -> Left ("the password command failed (" <> T.pack (show n) <> ")")
      Left e -> Left ("cannot run the password command: " <> T.pack (show (e :: E.IOException)))
  where
    nonEmpty msg t = case T.strip (T.takeWhile (/= '\n') t) of
      "" -> Left msg
      p -> Right p

expandHome :: FilePath -> IO FilePath
expandHome ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandHome path = pure path

fetchAccount :: Integer -> Day -> Account -> IO (Either Text [Event])
fetchAccount horizon today = \case
  ICloud user secret -> withSecret secret $ \p -> caldav (Just (T.pack user, p)) "https://caldav.icloud.com/"
  Google user secret -> withSecret secret $ \p ->
    hint <$> caldav (Just (T.pack user, p)) ("https://www.google.com/calendar/dav/" <> T.pack user <> "/events/")
  CalDav url user secret -> withSecret secret $ \p -> caldav (Just (T.pack user, p)) (T.pack url)
  IcsUrl address -> do
    -- webcal:// is just https:// under another name
    let url = maybe (T.pack address) ("https://" <>) (T.stripPrefix "webcal://" (T.pack address))
    response <- http (Request "GET" url Nothing Nothing Nothing)
    case response of
      Left err -> pure (Left err)
      Right (status, body)
        | status == 200 -> Right <$> resolve horizon today Nothing (parseIcal (decode body))
        | otherwise -> pure (Left (statusError status))
  where
    withSecret secret act = readSecret secret >>= either (pure . Left) act
    caldav auth base = do
      found <- discover auth base
      -- a URL that is not an account root is taken as the calendar itself
      let calendars = case found of
            Right cs@(_ : _) -> cs
            _ -> [(base, Nothing)]
      results <- forM calendars $ \(url, tint) -> do
        objects <- query horizon auth url today
        either (pure . Left) (fmap Right . resolve horizon today tint . concatMap parseIcal) objects
      pure $ case (rights results, lefts results, found) of
        (events@(_ : _), _, _) -> Right (concat events)
        (_, _, Left err) -> Left err
        (_, err : _, _) -> Left err
        ([], [], _) -> Right []
    hint (Left err) = Left (err <> " — if Google rejects the app password, add the calendar's secret iCal address instead")
    hint ok = ok

------------------------------------------------------------------------------
-- CalDAV

-- | The calendars of an account that hold events, with their colours:
-- principal, then its calendar home, then the collections in it.
discover :: Maybe (Text, Text) -> Text -> IO (Either Text [(Text, Maybe String)])
discover auth base = do
  principal <- propfind base "0" "<d:current-user-principal/>" (href "current-user-principal")
  case principal of
    Left err -> pure (Left err)
    Right Nothing -> pure (Right [])
    Right (Just principalUrl) -> do
      home <- propfind principalUrl "0" "<c:calendar-home-set/>" (href "calendar-home-set")
      case home of
        Left err -> pure (Left err)
        Right Nothing -> pure (Right [])
        Right (Just homeUrl) ->
          propfind homeUrl "1"
            "<d:resourcetype/><d:displayname/><a:calendar-color/><c:supported-calendar-component-set/>"
            (\url doc -> mapMaybe (collection url) (named "response" doc))
  where
    propfind url depth props extract = do
      let body = "<d:propfind xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\" xmlns:a=\"http://apple.com/ns/ical/\"><d:prop>"
            <> props <> "</d:prop></d:propfind>"
      response <- http (Request "PROPFIND" url auth (Just depth) (Just body))
      pure $ case response of
        Left err -> Left err
        Right (status, bytes)
          | status == 207, Just doc <- parseXml (decode bytes) -> Right (extract url doc)
          | status == 207 -> Left "unreadable answer from the server"
          | otherwise -> Left (statusError status)
    href name url doc = absolute url . text <$> listToMaybe (named name doc >>= named "href")
    collection url response = do
      link <- listToMaybe (named "href" response)
      let components = mapMaybe (attr "name") (named "comp" response)
          isCalendar = not (null (named "resourcetype" response >>= named "calendar"))
      if isCalendar && (null components || "VEVENT" `elem` components)
        then Just (absolute url (text link), listToMaybe (named "calendar-color" response) >>= colour . text)
        else Nothing

-- | The iCalendar objects of a calendar with events in the next @horizon@
-- days. The server is asked to expand repeating events; if it does not,
-- 'occurrences' does it here.
query :: Integer -> Maybe (Text, Text) -> Text -> Day -> IO (Either Text [Text])
query horizon auth url today = do
  let utcStamp d = T.pack (formatTime defaultTimeLocale "%Y%m%dT000000Z" d)
      range = "start=\"" <> utcStamp (addDays (-1) today) <> "\" end=\"" <> utcStamp (addDays (horizon + 1) today) <> "\""
      body = "<c:calendar-query xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">"
        <> "<d:prop><c:calendar-data><c:expand " <> range <> "/></c:calendar-data></d:prop>"
        <> "<c:filter><c:comp-filter name=\"VCALENDAR\"><c:comp-filter name=\"VEVENT\"><c:time-range "
        <> range <> "/></c:comp-filter></c:comp-filter></c:filter></c:calendar-query>"
  response <- http (Request "REPORT" url auth (Just "1") (Just body))
  pure $ case response of
    Left err -> Left err
    Right (status, bytes)
      | status == 207, Just doc <- parseXml (decode bytes) -> Right (map text (named "calendar-data" doc))
      | status == 207 -> Left "unreadable answer from the server"
      | otherwise -> Left (statusError status)

text :: Element -> Text
text = T.strip . content

-- | A link from an answer, made absolute against the URL that was asked.
absolute :: Text -> Text -> Text
absolute base link
  | "http://" `T.isPrefixOf` link || "https://" `T.isPrefixOf` link = link
  | otherwise = origin <> (if "/" `T.isPrefixOf` link then link else "/" <> link)
  where
    (scheme, rest) = T.breakOn "://" base
    origin = scheme <> "://" <> T.takeWhile (/= '/') (T.drop 3 rest)

-- "#rrggbb" or "#rrggbbaa", as "#rrggbb".
colour :: Text -> Maybe String
colour t = case T.unpack t of
  c@('#' : hex) | length hex >= 6, all (`elem` ("0123456789abcdefABCDEF" :: String)) (take 6 hex) -> Just (take 7 c)
  _ -> Nothing

statusError :: Int -> Text
statusError = \case
  401 -> "wrong user or password (401)"
  403 -> "access denied (403)"
  404 -> "address not found (404)"
  status -> "the server answered " <> T.pack (show status)

decode :: B.ByteString -> Text
decode = TE.decodeUtf8With lenientDecode

------------------------------------------------------------------------------
-- HTTP

data Request = Request
  { rqMethod :: Text
  , rqUrl    :: Text
  , rqAuth   :: Maybe (Text, Text)
  , rqDepth  :: Maybe Text
  , rqBody   :: Maybe Text
  }

-- | Runs curl and returns the HTTP status and the body. Everything, the
-- password included, goes to curl through its standard input as a config
-- file, never on its command line, where other users could see it.
http :: Request -> IO (Either Text (Int, B.ByteString))
http rq = do
  result <- E.try (run >>= E.evaluate)
  pure $ case result of
    Left e -> Left ("could not run curl: " <> T.pack (show (e :: E.IOException)))
    Right (ExitSuccess, out, _)
      | (body, status) <- BC.breakEnd (== '\n') out
      , Just (code, _) <- BC.readInt status -> Right (code, B.dropEnd 1 body)
    Right (ExitSuccess, _, _) -> Left "unreadable answer from curl"
    Right (ExitFailure _, _, err) -> Left (T.strip (T.takeWhile (/= '\n') (decode err)))
  where
    config = T.unlines . catMaybes $
      [ Just ("url = " <> quoted (rqUrl rq))
      , Just ("request = " <> quoted (rqMethod rq))
      , (\(user, password) -> "user = " <> quoted (user <> ":" <> password)) <$> rqAuth rq
      , (\depth -> "header = " <> quoted ("Depth: " <> depth)) <$> rqDepth rq
      , (\body -> "data-binary = " <> quoted body) <$> rqBody rq
      , ("header = " <> quoted "Content-Type: application/xml; charset=utf-8") <$ rqBody rq
      , Just "user-agent = \"waymobar\""
      , Just "location"
      , Just "max-time = 30"
      , Just "write-out = \"\\n%{http_code}\""
      ]
    quoted t = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" t) <> "\""
    run = withCreateProcess
      (proc "curl" ["-sS", "-K", "-"]) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      $ \stdin stdout stderr' process -> case (stdin, stdout, stderr') of
        (Just hin, Just hout, Just herr) -> do
          B.hPut hin (TE.encodeUtf8 config)
          hClose hin
          out <- B.hGetContents hout
          err <- B.hGetContents herr
          code <- waitForProcess process
          pure (code, out, err)
        _ -> pure (ExitFailure 1, "", "no pipes")

------------------------------------------------------------------------------
-- From the file to the bar

foreign import ccall unsafe "waymobar_zone_to_utc"
  c_zone_to_utc :: CString -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr Int64 -> IO CInt

-- | A wall-clock time in a named zone; 'Nothing' for a name the system
-- does not know.
zoneToUtc :: Text -> LocalTime -> IO (Maybe UTCTime)
zoneToUtc name (LocalTime day (TimeOfDay h m s)) =
  withCString (T.unpack name) $ \cname -> alloca $ \out -> do
    let (y, mo, d) = toGregorian day
    ok <- c_zone_to_utc cname (fromIntegral y) (fromIntegral mo) (fromIntegral d)
            (fromIntegral h) (fromIntegral m) (floor s) out
    if ok /= 0 then Just . posixSecondsToUTCTime . fromIntegral <$> peek out else pure Nothing

-- | Turns raw events into the ones to show: repeats expanded, times moved
-- to the system's zone, and only those touching the next @horizon@ days.
resolve :: Integer -> Day -> Maybe String -> [RawEvent] -> IO [Event]
resolve horizon today tint raw = fmap catMaybes . forM candidates $ \(e, at) -> do
  start <- toLocal (reStart e) at
  end <- toLocal (reStart e) (addLocalTime (lasts e) at)
  pure $
    if (end > windowStart || (start >= windowStart && end == start)) && start < windowEnd
      then Just (Event start end (allDay e) (title e) tint)
      else Nothing
  where
    candidates =
      [ (e, at)
      | e <- raw
      , not (reCancelled e)
      , at <- occurrences (addDays (horizon + 1) today) e
        -- the original of a replaced occurrence is left out; the replacement stays
      , null (reRule e) || (reUid e, at) `notElem` replaced
        -- far from the window in any time zone: not worth converting
      , localDay at <= addDays (horizon + 1) today
      , localDay (addLocalTime (lasts e) at) >= addDays (-1) today
      ]
    -- occurrences replaced by another event (one with RECURRENCE-ID)
    replaced = [ (reUid e, at) | e <- raw, Just at <- [reRecurrenceId e] ]
    toLocal reference wall = case reference of
      Utc _ -> fromUtc (localTimeToUTC utc wall)
      -- an unknown zone name: its times are read as local ones
      Zoned name _ -> zoneToUtc name wall >>= maybe (pure wall) fromUtc
      _ -> pure wall
    fromUtc t = (`utcToLocalTime` t) <$> getTimeZone t
    allDay e = case reStart e of
      DateOnly _ -> True
      _ -> False
    -- how long it lasts, in wall-clock terms; an all-day event with no end lasts a day
    lasts e = case (reEnd e, reDuration e) of
      (Just end, _) -> diffLocalTime (wallClock end) (wallClock (reStart e))
      (_, Just d) -> d
      _ | allDay e -> nominalDay
      _ -> 0
    title e = if T.null (reSummary e) then "(no title)" else T.unpack (reSummary e)
    windowStart = LocalTime today midnight
    windowEnd = LocalTime (addDays horizon today) midnight

------------------------------------------------------------------------------
-- The popover

-- | The agenda as popover markup: the date, then the next events under
-- "Today", "Tomorrow" or their day, each with a dot in its calendar's
-- colour and its time.
agendaMarkup :: Agenda -> String
agendaMarkup a = intercalate "\n" (("<b>" ++ escape (longDate today) ++ "</b>") : body)
  where
    today = localDay (agendaNow a)
    longDate = dropDoubleSpace . formatTime defaultTimeLocale "%A, %e %B"
    shown = take 10 (agendaEvents a)
    hidden = length (agendaEvents a) - length shown
    -- an event that started on an earlier day is listed under today
    dayOf e = max today (localDay (eventStart e))
    problems = [ dim ("⚠ " ++ escape (shorten 70 err)) | err <- take 1 (agendaErrors a) ]
    body
      | not (agendaLoaded a) = [dim "Loading…"]
      | null shown = if null problems then [dim "No upcoming events"] else problems
      | otherwise =
          concat [ gap : dim (dayLabel d) : map line (filter ((== d) . dayOf) shown) | d <- nub (map dayOf shown) ]
            ++ [ dim ("+" ++ show hidden ++ " more") | hidden > 0 ]
            ++ problems
    gap = "<span size='40%'> </span>"
    dayLabel d
      | d == today = "Today"
      | d == addDays 1 today = "Tomorrow"
      | otherwise = escape (dropDoubleSpace (formatTime defaultTimeLocale "%a %e %b" d))
    line e = dot e ++ "  " ++ dim (time e) ++ "\t" ++ escape (shorten 48 (eventTitle e))
    dot e = case eventColor e of
      Just c -> "<span foreground='" ++ c ++ "'>●</span>"
      Nothing -> dim "●"
    time e
      | eventAllDay e = "all day"
      | localDay (eventStart e) < today = "now"
      | otherwise = formatTime defaultTimeLocale "%H:%M" (eventStart e)
    dim s = "<span alpha='60%'>" ++ s ++ "</span>"
    -- %e pads the day with a space
    dropDoubleSpace = unwords . filter (not . all isSpace) . words
