-- | A tiny JSON encoder/parser, enough for the rivermonad IPC socket
-- (avoids depending on aeson).
module Waymobar.JSON
  ( JValue (..)
  , render
  , renderUtf8
  , parse
  , parseUtf8
  , lookupKey
  , str
  , obj
  ) where

import qualified Data.ByteString as B
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (readHex, showHex)
import Text.ParserCombinators.ReadP

data JValue
  = JNull
  | JBool Bool
  | JNum Double
  | JStr String
  | JArr [JValue]
  | JObj [(String, JValue)]
  deriving (Show, Eq)

str :: String -> JValue
str = JStr

obj :: [(String, JValue)] -> JValue
obj = JObj

lookupKey :: String -> JValue -> Maybe JValue
lookupKey k (JObj kvs) = lookup k kvs
lookupKey _ _ = Nothing

------------------------------------------------------------------------------
-- Encoding

render :: JValue -> String
render v = case v of
  JNull -> "null"
  JBool b -> if b then "true" else "false"
  JNum n
    | n == fromIntegral (round n :: Integer) -> show (round n :: Integer)
    | otherwise -> show n
  JStr s -> renderString s
  JArr xs -> "[" ++ intercalate "," (map render xs) ++ "]"
  JObj kvs -> "{" ++ intercalate "," [ renderString k ++ ":" ++ render x | (k, x) <- kvs ] ++ "}"

renderString :: String -> String
renderString s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c
      | ord c < 0x20 = "\\u" ++ pad4 (showHex (ord c) "")
      | otherwise = [c]
    pad4 h = replicate (4 - length h) '0' ++ h

renderUtf8 :: JValue -> B.ByteString
renderUtf8 = TE.encodeUtf8 . T.pack . render

------------------------------------------------------------------------------
-- Parsing

parse :: String -> Maybe JValue
parse s = case [ v | (v, rest) <- readP_to_S (skipSpaces *> value <* skipSpaces) s, null rest ] of
  (v : _) -> Just v
  [] -> Nothing

parseUtf8 :: B.ByteString -> Maybe JValue
parseUtf8 = parse . T.unpack . TE.decodeUtf8Lenient

value :: ReadP JValue
value = skipSpaces *> choice
  [ JNull <$ string "null"
  , JBool True <$ string "true"
  , JBool False <$ string "false"
  , JStr <$> stringLit
  , JNum <$> number
  , JArr <$> between (char '[' *> skipSpaces) (skipSpaces *> char ']') (sepBy value (skipSpaces *> char ','))
  , JObj <$> between (char '{' *> skipSpaces) (skipSpaces *> char '}') (sepBy member (skipSpaces *> char ','))
  ] <* skipSpaces
  where
    member = do
      skipSpaces
      k <- stringLit
      skipSpaces *> char ':' *> skipSpaces
      v <- value
      pure (k, v)

stringLit :: ReadP String
stringLit = char '"' *> many ch <* char '"'
  where
    ch = (satisfy (\c -> c /= '"' && c /= '\\')) +++ (char '\\' *> escape)
    escape = choice
      [ '"' <$ char '"', '\\' <$ char '\\', '/' <$ char '/', '\b' <$ char 'b'
      , '\f' <$ char 'f', '\n' <$ char 'n', '\r' <$ char 'r', '\t' <$ char 't'
      , char 'u' *> (hex4 >>= \c -> surrogate c +++ pure (chr c)) ]
    hex4 = do
      ds <- count 4 (satisfy isHexDigit)
      case readHex ds of
        [(n, "")] -> pure n
        _ -> pfail
    -- UTF-16 surrogate pairs (😀)
    surrogate hi
      | hi >= 0xd800 && hi < 0xdc00 = do
          lo <- string "\\u" *> hex4
          if lo >= 0xdc00 && lo < 0xe000
            then pure (chr (0x10000 + (hi - 0xd800) * 0x400 + (lo - 0xdc00)))
            else pfail
      | otherwise = pfail

number :: ReadP Double
number = do
  sign <- option "" (string "-")
  int <- munch1 isDigit
  frac <- option "" ((:) <$> char '.' <*> munch1 isDigit)
  ex <- option "" $ do
    e <- satisfy (`elem` "eE")
    sg <- option "" ((: []) <$> satisfy (`elem` "+-"))
    ds <- munch1 isDigit
    pure (e : sg ++ ds)
  let txt = sign ++ int ++ (if null frac then "" else frac) ++ ex
  case reads (if null frac && not (null ex) then sign ++ int ++ ".0" ++ ex else txt) of
    [(n, "")] -> pure n
    _ -> pfail
