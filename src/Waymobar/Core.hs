-- | The types a configuration is made of.
module Waymobar.Core
  ( -- * Configuration
    Config (..)
  , Position (..)
  , Layer (..)
  , defaultConfig
    -- * Widgets
  , Widget (..)
  , BarEnv (..)
  , Output
  , Block (..)
  , block
  , Click (..)
  , Button (..)
  , MenuEntry (..)
  , menuItem
  , Icon
  , IconSource (..)
  , Style (..)
  , noStyle
  , styled
  ) where

import Waymobar.Draw (Icon)
import Waymobar.Rivermonad (Action, RState)

data Position = Top | Bottom
  deriving (Eq, Show)

-- | The layer-shell layer the bar lives in.
data Layer = BackgroundLayer | BottomLayer | TopLayer | OverlayLayer
  deriving (Eq, Show)

data Config = Config
  { font          :: String    -- ^ pango font description, e.g. @"monospace 10"@
  , barHeight     :: Int       -- ^ in logical pixels; 0 = fit the font
  , position      :: Position
  , layer         :: Layer
  , exclusive     :: Bool      -- ^ reserve the bar's space (tiled windows avoid it)
  , margins       :: (Int, Int, Int, Int)
    -- ^ space around the bar (top, right, bottom, left), in logical pixels.
    -- Match rivermonad's outer gap to line the bar up with the windows,
    -- e.g. @(1, 1, 0, 1)@ with @gapsWithEdge 1@.
  , bgColor       :: String    -- ^ @#rrggbb@ or @#rrggbbaa@
  , fgColor       :: String
  , left          :: [Widget]
  , center        :: [Widget]
  , right         :: [Widget]
  , widgetSpacing :: Int       -- ^ pixels between widgets
  , edgePadding   :: Int       -- ^ pixels before the first and after the last widget
  , outputs       :: [String]  -- ^ outputs to show a bar on (by name, e.g. @"DP-1"@); [] = all
  , namespace     :: String    -- ^ layer-shell namespace, for compositor rules
  , menuHighlight :: String    -- ^ background of the menu entry under the pointer
  , menuBorder    :: String    -- ^ border of menus
  }

-- | An empty bar; "Waymobar.Config" has a populated 'def'.
defaultConfig :: Config
defaultConfig = Config
  { font = "monospace 10"
  , barHeight = 0
  , position = Top
  , layer = TopLayer
  , exclusive = True
  , margins = (0, 0, 0, 0)
  , bgColor = "#2e3440"
  , fgColor = "#d8dee9"
  , left = []
  , center = []
  , right = []
  , widgetSpacing = 12
  , edgePadding = 4
  , outputs = []
  , namespace = "waymobar"
  , menuHighlight = "#434c5e"
  , menuBorder = "#4c566a"
  }

-- | An output name, such as @"DP-1"@ (empty if the compositor did not say).
type Output = String

data Button = LeftButton | MiddleButton | RightButton | ScrollUp | ScrollDown | OtherButton Int
  deriving (Eq, Show)

data Click = Click
  { clickButton :: Button
  , clickOutput :: Output              -- ^ the output of the bar that was clicked
  , clickArea   :: (Double, Double)    -- ^ left and right edge of the clicked block
  , clickMenu   :: [MenuEntry] -> IO () -- ^ open a menu under the clicked block
  }

-- | An entry of a popup menu (see 'clickMenu').
data MenuEntry
  = MenuItem
      { itemLabel   :: String       -- ^ plain text
      , itemEnabled :: Bool
      , itemToggle  :: Maybe Bool   -- ^ a check mark (Just True = checked)
      , itemIcon    :: Maybe Icon
      , itemAction  :: IO ()
      }
  | Submenu String [MenuEntry]
  | MenuSeparator

menuItem :: String -> IO () -> MenuEntry
menuItem l = MenuItem l True Nothing Nothing

-- | Colours of a block; 'Nothing' keeps the bar's.
data Style = Style
  { styleFg        :: Maybe String
  , styleBg        :: Maybe String
  , styleUnderline :: Maybe String  -- ^ a line at the bottom edge
  , styleOverline  :: Maybe String  -- ^ a line at the top edge
  } deriving (Eq, Show)

noStyle :: Style
noStyle = Style Nothing Nothing Nothing Nothing

-- | Where an icon comes from. It is rasterized when the bar is drawn, at
-- the output's real pixel size, so SVGs stay sharp at any scale.
data IconSource
  = IconFile FilePath            -- ^ an image file: SVG, PNG... (@~/@ is expanded)
  | IconName [FilePath] String   -- ^ a freedesktop icon theme name, extra directories searched first
  deriving (Eq, Ord, Show)

-- | One rectangle of a bar: an optional icon followed by some pango markup,
-- with colours and an optional click handler.
data Block = Block
  { blockMarkup         :: String
  , blockStyle          :: Style
  , blockPadding        :: Int     -- ^ pixels at each side
  , blockMinWidth       :: Int
  , blockOnClick        :: Maybe (Click -> IO ())
  , blockIcon           :: Maybe Icon        -- ^ a ready-made bitmap (fallback for 'blockIconSource')
  , blockIconSize       :: Int     -- ^ logical pixels; 0 = the bar height minus 8
  , blockIconSource     :: Maybe IconSource  -- ^ preferred over 'blockIcon' when it loads
  , blockIconColor      :: Maybe String      -- ^ colour for symbolic icons; 'Nothing' = the text colour
  , blockIconRecolorAll :: Bool              -- ^ recolour every icon, not only @-symbolic@ ones
  }

-- | A block showing some markup, without padding or click handler.
block :: String -> Block
block m = Block m noStyle 0 0 Nothing Nothing 0 Nothing Nothing False

styled :: Style -> Block -> Block
styled s b = b { blockStyle = s }

-- | What a widget can use.
data BarEnv = BarEnv
  { redraw          :: IO ()                  -- ^ ask every bar to redraw
  , rivermonadState :: IO (Maybe RState)      -- ^ 'Nothing' if rivermonad isn't reachable
  , rivermonadDo    :: Action -> IO ()
  }

-- | A widget is started once, when the bar starts (it may fork threads
-- that update its contents and call 'redraw'). It returns what to draw on
-- a given output, which is called on every redraw and must be quick.
newtype Widget = Widget { startWidget :: BarEnv -> IO (Output -> IO [Block]) }
