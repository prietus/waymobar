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
  , module Waymobar.Tray
  , module Waymobar.Audio
  , module Waymobar.Network
  , module Waymobar.Media
  , module Waymobar.Weather
  , R.Action (..)
  , R.Workspace (..)
  , R.RState (..)
  , R.sendAction
  ) where

import Waymobar.Bar (runBar)
import Waymobar.Config (def)
import Waymobar.Core
import qualified Waymobar.Rivermonad as R
import Waymobar.Widgets
import Waymobar.Tray
import Waymobar.Audio
import Waymobar.Network
import Waymobar.Media
import Waymobar.Weather

-- | Run a bar with this configuration on every output.
waymobar :: Config -> IO ()
waymobar = runBar
