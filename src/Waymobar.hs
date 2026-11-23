-- | waymobar: a status bar for Wayland written and configured in Haskell,
-- in the spirit of xmobar, with native support for rivermonad.
--
-- > import Waymobar
-- >
-- > main :: IO ()
-- > main = waymobar def
-- >   { font = "JetBrains Mono 10"
-- >   , right = [ clock "%H:%M" ]
-- >   }
module Waymobar
  ( waymobar
  , def
  , module Waymobar.Core
  , module Waymobar.Widgets
  , module Waymobar.Taskbar
  , module Waymobar.Tray
  , module Waymobar.Audio
  , module Waymobar.Network
  , module Waymobar.Notifications
  , module Waymobar.Media
  , module Waymobar.Weather
  , module Waymobar.GeoClue
  , module Waymobar.Calendar
  , module Waymobar.Apps
  , module Waymobar.Power
  , R.Action (..)
  , R.Workspace (..)
  , R.WindowEntry (..)
  , R.RState (..)
  , R.sendAction
  ) where

import Waymobar.Bar (runBar)
import Waymobar.Config (def)
import Waymobar.Core
import qualified Waymobar.Rivermonad as R
import Waymobar.Widgets
import Waymobar.Taskbar
import Waymobar.Tray
import Waymobar.Audio
import Waymobar.Network
import Waymobar.Notifications
import Waymobar.Media
import Waymobar.Weather
import Waymobar.GeoClue
import Waymobar.Calendar
import Waymobar.Apps
import Waymobar.Power

-- | Run a bar with this configuration on every output.
waymobar :: Config -> IO ()
waymobar = runBar
