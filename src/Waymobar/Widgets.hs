-- | Ready-made widgets and widget combinators.
module Waymobar.Widgets
  ( -- * Generic
    text
  , periodic
  , periodicIO
  , periodicHoverIO
  , clock
  , command
  , commandMarkup
  , commandReader
  , readCommand
    -- * System
  , cpu
  , memory
  , battery
    -- * rivermonad
  , WorkspacesConfig (..)
  , defaultWorkspaces
  , workspaces
  , layoutName
  , windowTitle
  , logoutButton
    -- * Icons
  , icon
  , withIcon
  , iconColor
  , withIconSize
  , iconSource
    -- * Combinators
  , onClick
  , spawnOnClick
  , withStyle
  , baseStyle
  , groupStyle
  , hoverStyle
  , colored
  , padded
  , markup
  , hoverMarkup
  , spawn
  , escape
  , shorten
  ) where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void, when)
import Data.Char (toLower)
import Data.IORef
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime, getCurrentTime, utctDayTime)
import System.Directory (listDirectory)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), hGetLine, hSetBuffering, hSetEncoding, utf8)
import System.Process

import Waymobar.Core
import Waymobar.Draw (escapeMarkup)
import Waymobar.Rivermonad

------------------------------------------------------------------------------
-- Generic

-- | Fixed pango markup.
text :: String -> Widget
text m = Widget $ \_ -> pure (const (pure [block m]))

-- | Markup recomputed every @secs@ seconds.
periodic :: Double -> IO String -> Widget
periodic secs act = periodicIO secs (pure act)

-- | Like 'periodic', with a setup action that can keep state between
-- samples (see 'cpu').
periodicIO :: Double -> IO (IO String) -> Widget
periodicIO secs setup = periodicHoverIO secs (fmap (\s -> (s, "")) <$> setup)

-- | Like 'periodicIO', but every sample is the markup of the bar and the
-- markup of the popover shown while the pointer is on the widget (@""@ =
-- no popover).
periodicHoverIO :: Double -> IO (IO (String, String)) -> Widget
periodicHoverIO secs setup = Widget $ \env -> do
  act <- setup
  ref <- newIORef ("", "")
  void $ forkIO $ forever $ do
    r <- E.try act
    let s = either (\e -> const ("", "") (e :: E.SomeException)) id r
    old <- atomicModifyIORef' ref (\o -> (s, o))
    unless (s == old) (redraw env)
    threadDelay (round (secs * 1000000))
  pure $ \_ -> (\(s, h) -> [ (block s) { blockHoverMarkup = if null h then Nothing else Just h } | not (null s) ]) <$> readIORef ref

-- | The local time, with a 'formatTime' format such as @"%a %d %b %H:%M"@.
-- Updates on the second.
clock :: String -> Widget
clock fmt = Widget $ \env -> do
  let now = formatTime defaultTimeLocale fmt <$> getZonedTime
  ref <- newIORef =<< now
  void $ forkIO $ forever $ do
    t <- utctDayTime <$> getCurrentTime
    let frac = t - fromIntegral (floor t :: Integer)
    threadDelay (max 1000 (round ((1 - realToFrac frac :: Double) * 1000000)))
    s <- now
    old <- atomicModifyIORef' ref (\o -> (s, o))
    unless (s == old) (redraw env)
  pure $ \_ -> (\s -> [block (escape s)]) <$> readIORef ref

-- | The first line printed by a shell command, every @secs@ seconds.
command :: String -> Double -> Widget
command cmd secs = periodic secs (escape <$> firstLine cmd)

-- | Like 'command', but the output is pango markup.
commandMarkup :: String -> Double -> Widget
commandMarkup cmd secs = periodic secs (firstLine cmd)

-- | The first line a shell command prints (for your own 'periodic' widgets).
readCommand :: String -> IO String
readCommand = firstLine

firstLine :: String -> IO String
firstLine cmd = do
  (_, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
  pure (takeWhile (/= '\n') out)

-- | A long-running command: shows the last line it printed (as pango
-- markup). It is restarted if it exits.
commandReader :: String -> Widget
commandReader cmd = Widget $ \env -> do
  ref <- newIORef ""
  void $ forkIO $ forever $ do
    _ <- E.try @E.SomeException $ do
      (_, Just out, _, ph) <- createProcess (shell cmd) { std_out = CreatePipe, new_session = True }
      hSetBuffering out LineBuffering
      hSetEncoding out utf8
      let loop = do
            l <- hGetLine out
            old <- atomicModifyIORef' ref (\o -> (l, o))
            unless (l == old) (redraw env)
            loop
      _ <- E.try @E.IOException loop
      void (waitForProcess ph)
    threadDelay 3000000
  pure $ \_ -> (\s -> [block s | not (null s)]) <$> readIORef ref

------------------------------------------------------------------------------
-- System

-- | CPU usage in percent, sampled every 2 seconds.
cpu :: (Int -> String) -> Widget
cpu fmt = periodicIO 2 $ do
  prev <- newIORef =<< sample
  pure $ do
    (busy, total) <- sample
    (pb, pt) <- atomicModifyIORef' prev (\o -> ((busy, total), o))
    let dt = total - pt
        pct = if dt <= 0 then 0 else round (100 * fromIntegral (busy - pb) / fromIntegral dt :: Double)
    pure (fmt pct)
  where
    sample :: IO (Integer, Integer)
    sample = do
      s <- readFileStrict "/proc/stat"
      case words <$> listToMaybe (lines s) of
        Just ("cpu" : ns) ->
          let vals = map read ns :: [Integer]
              idle = sum (take 2 (drop 3 vals))   -- idle + iowait
              total = sum vals
          in pure (total - idle, total)
        _ -> pure (0, 0)

-- | Memory in use, in percent, every 5 seconds.
memory :: (Int -> String) -> Widget
memory fmt = periodic 5 $ do
  s <- readFileStrict "/proc/meminfo"
  let field k = listToMaybe [ read v :: Integer | (k' : v : _) <- map words (lines s), k' == k ]
  pure $ case (field "MemTotal:", field "MemAvailable:") of
    (Just t, Just a) | t > 0 -> fmt (fromIntegral ((t - a) * 100 `div` t))
    _ -> ""

-- | The first battery: capacity in percent and status (@"Charging"@,
-- @"Discharging"@, @"Full"@, ...), every 10 seconds. Empty without battery.
battery :: (Int -> String -> String) -> Widget
battery fmt = periodic 10 $ do
  let dir = "/sys/class/power_supply/"
  names <- either (const []) id <$> E.try @E.IOException (listDirectory dir)
  bats <- flip filterIO names $ \n -> do
    t <- E.try @E.IOException (readFileStrict (dir ++ n ++ "/type"))
    pure (either (const False) ((== "Battery") . trim) t)
  case bats of
    [] -> pure ""
    (b : _) -> do
      cap <- trim <$> readFileStrict (dir ++ b ++ "/capacity")
      st <- trim <$> readFileStrict (dir ++ b ++ "/status")
      pure (fmt (fromMaybe 0 (readMaybe cap)) st)
  where
    filterIO p = fmap concat . mapM (\x -> (\ok -> [x | ok]) <$> p x)
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

------------------------------------------------------------------------------
-- rivermonad

data WorkspacesConfig = WorkspacesConfig
  { wsLabel      :: Workspace -> String  -- ^ markup for a workspace (default: its name)
  , focusedStyle :: Style   -- ^ the focused workspace, on the focused output's bar
  , activeStyle  :: Style   -- ^ shown on this bar's output
  , visibleStyle :: Style   -- ^ shown on another output
  , hiddenStyle  :: Style   -- ^ not shown, has windows
  , emptyStyle   :: Style   -- ^ not shown, no windows
  , hideEmpty    :: Bool    -- ^ like xmonad's @ppHiddenNoWindows = const ""@
  , showExtra    :: Bool    -- ^ also list workspaces outside the configured ones (e.g. the scratchpad's @NSP@)
  , wsPadding    :: Int
  , clickAction  :: Workspace -> Action  -- ^ on left click (default: 'FocusWorkspace')
  , scrollCycles :: Bool    -- ^ the mouse wheel moves to the next/previous workspace
  }

defaultWorkspaces :: WorkspacesConfig
defaultWorkspaces = WorkspacesConfig
  { wsLabel = escape . wsName
  , focusedStyle = noStyle { styleFg = Just "#2e3440", styleBg = Just "#88c0d0" }
  , activeStyle = noStyle { styleUnderline = Just "#88c0d0" }
  , visibleStyle = noStyle { styleFg = Just "#ebcb8b" }
  , hiddenStyle = noStyle
  , emptyStyle = noStyle { styleFg = Just "#4c566a" }
  , hideEmpty = False
  , showExtra = False
  , wsPadding = 6
  , clickAction = FocusWorkspace . wsName
  , scrollCycles = True
  }

-- | One clickable block per workspace. Like in xmonad, workspaces are not
-- tied to outputs: every bar lists all of them.
workspaces :: WorkspacesConfig -> Widget
workspaces cfg = Widget $ \env -> pure $ \out -> do
  mst <- rivermonadState env
  pure $ case mst of
    Nothing -> []
    Just st ->
      let wss = rsWorkspaces st
          here w = wsOutput w == Just out
          -- Runtime workspaces (the scratchpad's NSP...) are rivermonad
          -- internals: skip them unless asked, also for the mouse wheel.
          shown = [ w | w <- wss, showExtra cfg || not (wsExtra w)
                      , not (hideEmpty cfg && kind w == Empty) ]
          kind w
            | wsFocused w && here w = Focused
            | here w = Active
            | wsVisible w = Visible
            | wsWindows w > 0 = Hidden
            | otherwise = Empty
          styleOf = \case
            Focused -> focusedStyle cfg
            Active -> activeStyle cfg
            Visible -> visibleStyle cfg
            Hidden -> hiddenStyle cfg
            Empty -> emptyStyle cfg
          -- the workspace this bar's output shows, for the mouse wheel
          cur = listToMaybe [ w | w <- shown, here w ]
          step d = do
            c <- cur
            let names = map wsName shown
                i = length (takeWhile (/= wsName c) names)
                n = length shown
            if n < 2 then Nothing else Just (shown !! ((i + d) `mod` n))
          click w c = case clickButton c of
            LeftButton -> rivermonadDo env (clickAction cfg w)
            ScrollUp | scrollCycles cfg -> mapM_ (rivermonadDo env . clickAction cfg) (step (-1))
            ScrollDown | scrollCycles cfg -> mapM_ (rivermonadDo env . clickAction cfg) (step 1)
            _ -> pure ()
      in [ (block (wsLabel cfg w)) { blockStyle = styleOf (kind w), blockPadding = wsPadding cfg, blockOnClick = Just (click w) } | w <- shown ]

data Kind = Focused | Active | Visible | Hidden | Empty
  deriving (Eq)

-- | The layout of the workspace on this bar's output; a click switches
-- to the next layout.
layoutName :: (String -> String) -> Widget
layoutName fmt = Widget $ \env -> pure $ \out -> do
  mst <- rivermonadState env
  pure $ case mst >>= \st -> listToMaybe [ w | w <- rsWorkspaces st, wsOutput w == Just out ] of
    Nothing -> []
    Just w ->
      [ (block (fmt (escape (wsLayout w))))
          { blockOnClick = Just $ \c -> when (clickButton c == LeftButton) (rivermonadDo env NextLayout) } ]

-- | The title of the focused window, cut to @n@ characters.
windowTitle :: Int -> Widget
windowTitle n = Widget $ \env -> pure $ \_ -> do
  mst <- rivermonadState env
  pure [ block (escape (shorten n t)) | Just t <- [mst >>= rsTitle], not (null t) ]

------------------------------------------------------------------------------
-- Icons

-- | Where an icon comes from: a path (absolute, @~/...@, or anything with a
-- @/@ or an image extension) is a file; anything else is an icon theme
-- name, e.g. @"audio-volume-high-symbolic"@.
iconSource :: String -> IconSource
iconSource s
  | '/' `elem` s || any (`isSuffixOf` map toLower s) [".svg", ".png", ".xpm", ".jpg", ".jpeg"] = IconFile s
  | otherwise = IconName [] s

-- | Just an icon. SVGs are drawn at the output's real resolution.
--
-- > icon "~/icons/wifi.svg"
-- > icon "network-wireless-symbolic"
icon :: String -> Widget
icon s = Widget $ \_ -> pure $ \_ -> pure [ (block "") { blockIconSource = Just (iconSource s) } ]

-- | Put an icon before the text of every block of a widget.
--
-- > withIcon "audio-volume-high-symbolic" volume
withIcon :: String -> Widget -> Widget
withIcon s = mapBlocks (\b -> b { blockIconSource = Just (iconSource s) })

-- | Recolour the widget's icons (all of them, not only @-symbolic@ ones),
-- handy for monochrome SVGs.
iconColor :: String -> Widget -> Widget
iconColor c = mapBlocks (\b -> b { blockIconColor = Just c, blockIconRecolorAll = True })

-- | Icon size in logical pixels (default: the bar height minus 8).
withIconSize :: Int -> Widget -> Widget
withIconSize n = mapBlocks (\b -> b { blockIconSize = n })

-- | A button that logs out of the rivermonad session after confirming in a
-- menu: every window is asked to close (so applications can save), then the
-- session ends and the display manager (GDM...) shows its login screen. If
-- a window refuses to close within 10 seconds, the logout is cancelled.
--
-- > logoutButton "\xf011"   -- nf-fa-power_off
logoutButton :: String -> Widget
logoutButton label = Widget $ \env -> pure $ \_ -> pure
  [ (block label)
      { blockOnClick = Just $ \c -> when (clickButton c == LeftButton) $ clickMenu c
          [ menuItem "Log out" (rivermonadDo env Logout)
          , MenuSeparator
          , menuItem "Cancel" (pure ()) ] } ]

------------------------------------------------------------------------------
-- Combinators

-- | Run an action when the widget is clicked (replaces its own handlers).
onClick :: (Click -> IO ()) -> Widget -> Widget
onClick h = mapBlocks (\b -> b { blockOnClick = Just h })

-- | Run a shell command on left click.
spawnOnClick :: String -> Widget -> Widget
spawnOnClick cmd = onClick (\c -> when (clickButton c == LeftButton) (spawn cmd))

-- | Style every block of a widget; the fields set in the given style win
-- over the block's own (e.g. a state colour of 'workspaces').
withStyle :: Style -> Widget -> Widget
withStyle s = mapBlocks (\b -> b { blockStyle = mergeStyle s (blockStyle b) })

-- | Like 'withStyle', but the block's own style wins: a common look under
-- per-state styles.
baseStyle :: Style -> Widget -> Widget
baseStyle s = mapBlocks (\b -> b { blockStyle = mergeStyle (blockStyle b) s })

-- | One box behind all the blocks of a widget (a "pill" around the
-- workspace buttons, say). Uses the background, border, radius, margin
-- and padding of the style.
groupStyle :: Style -> Widget -> Widget
groupStyle s = mapBlocks (\b -> b { blockGroupStyle = Just (maybe s (mergeStyle s) (blockGroupStyle b)) })

-- | Style merged over the blocks while the pointer is on them (CSS :hover).
hoverStyle :: Style -> Widget -> Widget
hoverStyle h = mapBlocks (\b -> b { blockStyle = mergeStyle noStyle { styleHover = Just h } (blockStyle b) })

-- | Set the text colour.
colored :: String -> Widget -> Widget
colored c = mapBlocks (\b -> b { blockStyle = (blockStyle b) { styleFg = Just c } })

-- | Add horizontal padding to every block.
padded :: Int -> Widget -> Widget
padded p = mapBlocks (\b -> b { blockPadding = blockPadding b + p })

-- | Transform the markup of every block, e.g. @markup ("cpu " ++)@.
markup :: (String -> String) -> Widget -> Widget
markup f = mapBlocks (\b -> b { blockMarkup = f (blockMarkup b) })

-- | Markup shown in a popover under the block while the pointer is on it,
-- computed from the block's own markup. It may have several lines.
--
-- > hoverMarkup (const "<b>Log out</b>\nends the session") (logoutButton "⏻")
hoverMarkup :: (String -> String) -> Widget -> Widget
hoverMarkup f = mapBlocks (\b -> b { blockHoverMarkup = Just (f (blockMarkup b)) })

mapBlocks :: (Block -> Block) -> Widget -> Widget
mapBlocks f (Widget start) = Widget $ \env -> do
  r <- start env
  pure (fmap (map f) . r)

-- | Start a shell command, detached from waymobar.
spawn :: String -> IO ()
spawn cmd = void $ forkIO $ void $ E.try @E.SomeException $ do
  (_, _, _, ph) <- createProcess (shell cmd) { new_session = True, close_fds = True }
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ -> pure ()

escape :: String -> String
escape = escapeMarkup

-- | Cut to @n@ characters, ending in "…" when cut.
shorten :: Int -> String -> String
shorten n s
  | length s <= n = s
  | n <= 1 = take n s
  | otherwise = take (n - 1) s ++ "…"

readFileStrict :: FilePath -> IO String
readFileStrict p = do
  s <- readFile p
  length s `seq` pure s

trim :: String -> String
trim = reverse . dropWhile (`elem` " \n\t") . reverse . dropWhile (`elem` " \n\t")
