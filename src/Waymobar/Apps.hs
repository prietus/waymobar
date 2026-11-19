-- | The installed applications (their @.desktop@ files) and a launcher
-- that shows them in a grid of icons, like GNOME's.
module Waymobar.Apps
  ( App (..)
  , listApps
  , launchApp
  , LauncherConfig (..)
  , defaultLauncher
  , appLauncher
  ) where

import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Control.Monad (forM, forM_, void, when)
import qualified Data.ByteString as B
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (catMaybes, fromMaybe, isJust)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, (</>))

import Waymobar.Core
import Waymobar.Icons (resolveIcon)
import Waymobar.Widgets (spawn)

-- | An application that can be launched.
data App = App
  { appId       :: String   -- ^ desktop file ID, e.g. @"org.gnome.Nautilus.desktop"@
  , appName     :: String   -- ^ in the language of the session when the file has it
  , appIcon     :: String   -- ^ icon name or path (@""@ = none)
  , appExec     :: String   -- ^ command line, field codes (@%U@...) still in it
  , appTerminal :: Bool     -- ^ runs in a terminal
  } deriving (Eq, Show)

-- | The applications a menu should show, sorted by name: those of
-- @~/.local/share/applications@ and of every @applications@ directory in
-- @$XDG_DATA_DIRS@ (Flatpak's too), where the first file of an ID wins.
-- Hidden ones (@NoDisplay@, @Hidden@, @OnlyShowIn@ another desktop, a
-- @TryExec@ that is not installed) are left out.
listApps :: IO [App]
listApps = do
  home <- getXdgDirectory XdgData ""
  dataDirs <- maybe ["/usr/local/share", "/usr/share"] (filter (not . null) . splitOn ':') <$> lookupEnv "XDG_DATA_DIRS"
  desktops <- maybe [] (map (map toLower) . splitOn ':') <$> lookupEnv "XDG_CURRENT_DESKTOP"
  langs <- languages
  files <- concat <$> forM (home : dataDirs) (\d -> desktopFiles (d </> "applications"))
  -- the first file with an ID decides, even to hide it
  let byId = M.fromListWith (\_ first -> first) [ (i, f) | (i, f) <- files ]
  apps <- forM (M.toList byId) $ \(i, f) -> do
    r <- E.try (readFileStrict f)
    case r of
      Left (_ :: E.SomeException) -> pure Nothing
      Right txt -> parseDesktop langs desktops i txt
  pure (sortOn (map toLower . appName) (catMaybes apps))

-- | @.desktop@ files under a directory, with their IDs (the path below it,
-- @/@ turned into @-@).
desktopFiles :: FilePath -> IO [(String, FilePath)]
desktopFiles root = go root
  where
    go dir = do
      ok <- doesDirectoryExist dir
      if not ok then pure [] else do
        entries <- map (dir </>) <$> listDirectory dir `E.catch` \(_ :: E.IOException) -> pure []
        fmap concat $ forM entries $ \e -> do
          sub <- doesDirectoryExist e
          if sub then go e
            else pure [ (map (\c -> if c == '/' then '-' else c) (makeRelative root e), e) | ".desktop" `isSuffixOf` e ]

-- | The language of the session, most specific first: @es_ES.UTF-8@ gives
-- @["es_ES", "es"]@.
languages :: IO [String]
languages = do
  vars <- mapM lookupEnv ["LC_ALL", "LC_MESSAGES", "LANG"]
  let l = case filter (not . null) (map (fromMaybe "") vars) of
        (v : _) -> takeWhile (`notElem` ".@") v
        [] -> "C"
      base = takeWhile (/= '_') l
  pure (filter (`notElem` ["", "C", "POSIX"]) [l, base])

parseDesktop :: [String] -> [String] -> String -> String -> IO (Maybe App)
parseDesktop langs desktops i txt = do
  let entry = M.fromList
        [ (trim k, trim (drop 1 v))
        | l <- takeWhile (not . isGroup) (drop 1 (dropWhile (/= "[Desktop Entry]") (map trim (lines txt))))
        , not ("#" `isPrefixOf` l)
        , let (k, v) = break (== '=') l
        , not (null v) ]
      get k = M.lookup k entry
      true k = get k == Just "true"
      list k = maybe [] (map (map toLower) . filter (not . null) . splitOn ';') (get k)
      localized k = case catMaybes [ get (k ++ "[" ++ l ++ "]") | l <- langs ] of
        (v : _) -> Just v
        [] -> get k
      onlyIn = list "OnlyShowIn"
      notIn = list "NotShowIn"
      shown = get "Type" == Just "Application"
        && not (true "NoDisplay") && not (true "Hidden")
        && (null onlyIn || any (`elem` onlyIn) desktops)
        && not (any (`elem` notIn) desktops)
  tryOk <- case get "TryExec" of
    Nothing -> pure True
    Just t | "/" `isPrefixOf` t -> doesFileExist t
           | otherwise -> isJust <$> findExecutable t
  pure $ case (localized "Name", get "Exec") of
    (Just name, Just exec) | shown && tryOk -> Just App
      { appId = i
      , appName = unescape name
      , appIcon = fromMaybe "" (get "Icon")
      , appExec = exec
      , appTerminal = true "Terminal" }
    _ -> Nothing
  where
    isGroup l = "[" `isPrefixOf` l

-- | Start an application, detached from waymobar: with @gtk-launch@ when it
-- is installed (it handles every detail of the spec), otherwise from its
-- @Exec@ line (in @terminal@ if it asks for one).
launchApp :: String -> App -> IO ()
launchApp terminal app = do
  gtk <- findExecutable "gtk-launch"
  case gtk of
    Just g -> spawn (unwords [g, quote (appId app)])
    Nothing -> do
      let cmd = stripCodes (appExec app)
      spawn (if appTerminal app then terminal ++ " -e " ++ cmd else cmd)
  where
    quote s = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"
    -- %f %U %i... are for files and icons: dropped; %% is a %
    stripCodes ('%' : '%' : rest) = '%' : stripCodes rest
    stripCodes ('%' : _ : rest) = stripCodes rest
    stripCodes (c : rest) = c : stripCodes rest
    stripCodes [] = []

data LauncherConfig = LauncherConfig
  { launcherColumns  :: Int
  , launcherRows     :: Int     -- ^ rows shown at once; the wheel scrolls the rest
  , launcherIconSize :: Int     -- ^ logical pixels
  , launcherTerminal :: String  -- ^ for applications that run in a terminal, without gtk-launch
  }

-- | Six columns, five rows, 48 px icons, @foot@.
defaultLauncher :: LauncherConfig
defaultLauncher = LauncherConfig 6 5 48 "foot"

-- | A block with this markup that opens a grid of the installed
-- applications on left click (read again every time, so new ones show).
--
-- > appLauncher defaultLauncher "<span font_family='Symbols Nerd Font'>\xf303</span>"   -- the Arch Linux logo
appLauncher :: LauncherConfig -> String -> Widget
appLauncher cfg label = Widget $ \_ -> do
  -- rasterize the icons ahead, so the first grid opens at once (the usual
  -- output scales: 1 and 2)
  void $ forkIO $ void $ E.try @E.SomeException $ do
    apps <- listApps
    forM_ [1, 2] $ \k -> forM_ apps $ \a -> forM_ (iconOf a) $ \src ->
      resolveIcon src (k * launcherIconSize cfg) Nothing False
  pure $ \_ -> pure
    [ (block label)
        { blockOnClick = Just $ \c -> when (clickButton c == LeftButton) $ do
            apps <- listApps
            clickGrid c IconGrid
              { gridColumns = launcherColumns cfg
              , gridRows = launcherRows cfg
              , gridIconSize = launcherIconSize cfg
              , gridEntries =
                  [ GridEntry (appName a) (iconOf a) (launchApp (launcherTerminal cfg) a) | a <- apps ] } } ]
  where
    iconOf a = case appIcon a of
      "" -> Nothing
      p@('/' : _) -> Just (IconFile p)
      n -> Just (IconName [] n)

------------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- \s \n \t \r \\ in string values.
unescape :: String -> String
unescape ('\\' : c : rest) = (case c of 's' -> ' '; 'n' -> ' '; 't' -> ' '; 'r' -> ' '; _ -> c) : unescape rest
unescape (c : rest) = c : unescape rest
unescape [] = []

-- | A file as UTF-8 (what the spec asks for), whatever the locale.
readFileStrict :: FilePath -> IO String
readFileStrict p = T.unpack . TE.decodeUtf8Lenient <$> B.readFile p
