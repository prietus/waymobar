-- | A task bar: the windows that are open, with their application's icon
-- and title, from rivermonad's IPC (rivermonad >= 0.4.3).
--
-- > , taskbar defaultTaskbar
--
-- A click focuses the window (a minimised one comes back to the current
-- workspace), the middle button closes it, and the right button opens a
-- menu to close it or move it to another workspace. The icon is the one
-- of the application's desktop file (matched by the window's app id, or
-- its @StartupWMClass@).
module Waymobar.Taskbar
  ( taskbar
  , TaskbarConfig (..)
  , TaskbarScope (..)
  , defaultTaskbar
  ) where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.IORef
import Data.Maybe (listToMaybe)

import Waymobar.Apps (App (..), appForAppId, listApps)
import Waymobar.Core
import Waymobar.Rivermonad (Action (..), RState (..), WindowEntry (..), Workspace (..))
import Waymobar.Widgets (escape, shorten)

data TaskbarScope
  = ThisWorkspace   -- ^ the windows of the workspace shown on this bar's output
  | AllWorkspaces   -- ^ every window, workspace by workspace
  deriving (Eq, Show)

data TaskbarConfig = TaskbarConfig
  { tbScope        :: TaskbarScope
  , tbShowHidden   :: Bool                  -- ^ also minimised windows (and waiting scratchpads)
  , tbLabel        :: WindowEntry -> String -- ^ markup after the icon (@""@: only the icon)
  , tbFocusedStyle :: Style
  , tbNormalStyle  :: Style
  , tbHiddenStyle  :: Style                 -- ^ minimised windows
  , tbIconSize     :: Int                   -- ^ logical pixels; 0 = the bar height minus 8
  , tbPadding      :: Int
  }

-- | The windows of this workspace plus the minimised ones; the title cut
-- to 24 characters; the focused one underlined.
defaultTaskbar :: TaskbarConfig
defaultTaskbar = TaskbarConfig
  { tbScope = ThisWorkspace
  , tbShowHidden = True
  , tbLabel = escape . shorten 24 . winTitle
  , tbFocusedStyle = noStyle { styleUnderline = Just "#88c0d0" }
  , tbNormalStyle = noStyle
  , tbHiddenStyle = noStyle { styleFg = Just "#4c566a" }
  , tbIconSize = 0
  , tbPadding = 6
  }

taskbar :: TaskbarConfig -> Widget
taskbar cfg = Widget $ \env -> do
  -- the installed applications, for the icons; read again now and then
  apps <- newIORef []
  void $ forkIO $ forever $ do
    r <- E.try listApps
    either (\e -> const (pure ()) (e :: E.SomeException)) (writeIORef apps) r
    redraw env
    threadDelay 120000000
  pure $ \out -> do
    mst <- rivermonadState env
    as <- readIORef apps
    pure $ case mst of
      Nothing -> []
      Just st ->
        let here = listToMaybe [ wsName w | w <- rsWorkspaces st, wsOutput w == Just out ]
            workspaceNames = [ wsName w | w <- rsWorkspaces st, not (wsExtra w) ]
            shown = [ w | w <- rsWindows st
                        , if winHidden w then tbShowHidden cfg
                          else tbScope cfg == AllWorkspaces || Just (winWorkspace w) == here ]
        in map (windowBlock env cfg as workspaceNames) shown

windowBlock :: BarEnv -> TaskbarConfig -> [App] -> [String] -> WindowEntry -> Block
windowBlock env cfg apps workspaceNames w =
  (block (tbLabel cfg w))
    { blockStyle = style
    , blockPadding = tbPadding cfg
    , blockIconSource = Just (IconName [] iconName)
    , blockIconSize = tbIconSize cfg
    , blockHoverMarkup = if null (winTitle w) then Nothing else Just (escape (winTitle w))
    , blockOnClick = Just click
    }
  where
    style
      | winFocused w = tbFocusedStyle cfg
      | winHidden w = tbHiddenStyle cfg
      | otherwise = tbNormalStyle cfg
    iconName = case appForAppId apps (winAppId w) of
      Just a | not (null (appIcon a)) -> appIcon a
      _ -> if null (winAppId w) then "application-x-executable" else winAppId w
    act = rivermonadDo env
    click c = case clickButton c of
      LeftButton -> act (FocusWindowId (winId w))
      MiddleButton -> act (CloseWindowId (winId w))
      RightButton -> clickMenu c
        [ menuItem "Focus" (act (FocusWindowId (winId w)))
        , Submenu "Move to Workspace"
            [ menuItem t (act (MoveWindowId (winId w) t)) | t <- workspaceNames, t /= winWorkspace w ]
        , MenuSeparator
        , menuItem "Close" (act (CloseWindowId (winId w))) ]
      _ -> pure ()
