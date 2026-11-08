-- | The default configuration.
module Waymobar.Config
  ( def
  ) where

import Waymobar.Core
import Waymobar.Tray
import Waymobar.Widgets

-- | Workspaces and layout on the left, the focused window's title in the
-- middle, CPU, memory, battery, tray and clock on the right.
def :: Config
def = defaultConfig
  { left =
      [ workspaces defaultWorkspaces
      , colored "#81a1c1" (layoutName (\l -> "[" ++ l ++ "]"))
      ]
  , center = [ windowTitle 80 ]
  , right =
      [ cpu (\p -> "cpu " ++ show p ++ "%")
      , memory (\p -> "mem " ++ show p ++ "%")
      , battery (\p st -> "bat " ++ show p ++ "%" ++ (if st == "Charging" then "+" else ""))
      , tray defaultTray
      , colored "#88c0d0" (clock "%a %d %b  %H:%M")
      ]
  }
