{-# LANGUAGE OverloadedStrings #-}
-- | A tiny XML reader, enough for the answers of a CalDAV server (avoids
-- depending on an XML library). Lenient: it never fails on a tag that is
-- not closed. Names are kept without their namespace prefix.
module Waymobar.XML
  ( Element (..)
  , Node (..)
  , parseXml
  , named
  , content
  , attr
  ) where

import Data.Char (chr, isSpace)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (readHex)

data Element = Element
  { elName     :: Text             -- ^ local name: @"href"@ for @<d:href>@
  , elAttrs    :: [(Text, Text)]
  , elChildren :: [Node]
  }

data Node = Elem Element | Chars Text

-- | The root element of a document.
parseXml :: Text -> Maybe Element
parseXml doc = listToMaybe [ e | Elem e <- finish (go [Element "" [] []] doc) ]
  where
    -- The stack of open elements, innermost first, their children reversed.
    go st t
      | T.null t = st
      | Just r <- T.stripPrefix "<!--" t = go st (after "-->" r)
      | Just r <- T.stripPrefix "<![CDATA[" t =
          let (c, rest) = T.breakOn "]]>" r in go (push (Chars c) st) (T.drop 3 rest)
      | Just r <- T.stripPrefix "<?" t = go st (after "?>" r)
      | Just r <- T.stripPrefix "<!" t = go st (after ">" r)
      | Just r <- T.stripPrefix "</" t = go (close st) (after ">" r)
      | Just r <- T.stripPrefix "<" t =
          let (tag, rest) = tagEnd r
              selfClosed = "/" `T.isSuffixOf` tag
              (name, attrs) = parseTag (if selfClosed then T.dropEnd 1 tag else tag)
              opened = Element name attrs [] : st
          in go (if selfClosed then close opened else opened) rest
      | otherwise =
          let (c, rest) = T.break (== '<') t in go (push (Chars (entities c)) st) rest
    after end = T.drop (T.length end) . snd . T.breakOn end
    push n (e : st) = e { elChildren = n : elChildren e } : st
    push _ [] = []
    close (e : st@(_ : _)) = push (Elem e { elChildren = reverse (elChildren e) }) st
    close st = st
    finish st = case st of
      [root] -> reverse (elChildren root)
      _ -> finish (close st)

-- The text of a tag, up to its @>@ (one inside a quoted value does not count).
tagEnd :: Text -> (Text, Text)
tagEnd t = (T.take n t, T.drop (n + 1) t)
  where
    n = scan Nothing (0 :: Int) (T.unpack t)
    scan _ i [] = i
    scan Nothing i ('>' : _) = i
    scan Nothing i (c : cs) | c == '"' || c == '\'' = scan (Just c) (i + 1) cs
    scan (Just q) i (c : cs) | c == q = scan Nothing (i + 1) cs
    scan q i (_ : cs) = scan q (i + 1) cs

parseTag :: Text -> (Text, [(Text, Text)])
parseTag tag = (local name, attrs (T.stripStart rest))
  where
    (name, rest) = T.break isSpace tag
    attrs t
      | T.null t = []
      | otherwise =
          let (k, r) = T.break (== '=') t
          in case T.uncons (T.stripStart (T.drop 1 r)) of
               Just (q, r') | q == '"' || q == '\'' ->
                 let (v, r'') = T.break (== q) r'
                 in (local (T.strip k), entities v) : attrs (T.stripStart (T.drop 1 r''))
               _ -> []

local :: Text -> Text
local = T.takeWhileEnd (/= ':')

-- @&lt;@, @&amp;@, @&#13;@, @&#x1F4C5;@...
entities :: Text -> Text
entities t
  | T.any (== '&') t = T.concat (pieces t)
  | otherwise = t
  where
    pieces s = case T.breakOn "&" s of
      (a, r)
        | T.null r -> [a]
        | (e, r') <- T.break (== ';') (T.drop 1 r), not (T.null r'), T.length e <= 10, Just c <- entity e ->
            a : T.singleton c : pieces (T.drop 1 r')
        | otherwise -> a : "&" : pieces (T.drop 1 r)
    entity e = case T.unpack e of
      "lt" -> Just '<'
      "gt" -> Just '>'
      "amp" -> Just '&'
      "quot" -> Just '"'
      "apos" -> Just '\''
      '#' : x : hex | x == 'x' || x == 'X', [(n, "")] <- readHex hex -> code n
      '#' : dec | [(n, "")] <- reads dec -> code n
      _ -> Nothing
    code :: Int -> Maybe Char
    code n = if n > 0 && n < 0x110000 && (n < 0xD800 || n > 0xDFFF) then Just (chr n) else Nothing

-- | Every element with this local name: the element itself or any one
-- inside it.
named :: Text -> Element -> [Element]
named name e = [ e | elName e == name ] ++ concat [ named name c | Elem c <- elChildren e ]

-- | All the text inside an element.
content :: Element -> Text
content e = T.concat (map node (elChildren e))
  where
    node (Chars c) = c
    node (Elem c) = content c

attr :: Text -> Element -> Maybe Text
attr name = lookup name . elAttrs
