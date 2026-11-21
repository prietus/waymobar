{-# LANGUAGE LambdaCase #-}
-- ~/.config/waymobar/waymobar.hs
--
-- waymobar rebuilds this file when it changes (like xmonad). Check it with
-- `waymobar --recompile`.
--
-- The icons are Material Symbols (https://fonts.google.com/icons), from
-- the variable font (ttf-material-symbols-variable on Arch). Without it
-- their names show as text: use plain labels instead ("cpu " ++ ...).
import Control.Monad (when)

import Waymobar

-- | The font of the bar. The icons scale with 'fontSize', so changing it
-- keeps the proportion between the text and the icons.
fontName :: String
fontName = "monospace"

fontSize :: Double   -- in points
fontSize = 10

main :: IO ()
main = waymobar def
  { font = fontName ++ " " ++ show fontSize
  , barHeight = 26
  , position = Top
  , bgColor = "#1d2021"
  , fgColor = "#ebdbb2"
    -- menus, popovers and the side panel
  , popoverBg = Just "#282828"
  , popoverRadius = 12
  , menuBorder = "#504945"
  , menuHighlight = "#3c3836"
  , left =
      [ workspaces defaultWorkspaces
          { hideEmpty = True
          , focusedStyle = noStyle { styleFg = Just "#1d2021", styleBg = Just "#fabd2f" }
          , activeStyle = noStyle { styleUnderline = Just "#fabd2f" }
          , wsLabel = \w -> escape (wsName w) ++ (if wsWindows w > 1 then "<sup>" ++ show (wsWindows w) ++ "</sup>" else "")
          }
      , colored "#83a598" (layoutName (\l -> sym "view_sidebar" ++ escape l))
      ]
  , center = [ windowTitle 70 ]
  , right =
        -- cable or Wi-Fi, and the download speed
      [ colored "#83a598" (network 3 (\case
          Nothing -> sym "wifi_off" ++ "offline"
          Just n -> sym (if netKind n == Wifi then "wifi" else "lan") ++ rate (netRxRate n)))
      , colored "#8ec07c" (cpu (\p -> sym "memory" ++ pct p))
      , colored "#d3869b" (memory (\p -> sym "memory_alt" ++ pct p))
      , command "uname -r" 3600
      , spawnOnClick "foot" (text (symbol "add"))
        -- 'clock' escapes its text, so the icon is added afterwards
      , colored "#fabd2f" (markup (sym "schedule" ++) (clock "%a %d %b  %H:%M:%S"))
      ]
    -- push the pointer against the right edge of the screen
  , sidePanel = Just sessionPanel
  }

-- | A percentage always three figures wide ("007%"), so the bar does not
-- move when it goes from 9% to 10% or to 100%.
pct :: Int -> String
pct p = figures p ++ "%"

-- | A speed (bytes per second) always as wide: three figures in KB/s, MB/s
-- or GB/s ("042 KB/s", "003 MB/s").
rate :: Double -> String
rate r = figures v ++ " " ++ [u] ++ "B/s"
  where
    (v, u) = case [ (n, c) | (k, c) <- [(1e3, 'K'), (1e6, 'M'), (1e9, 'G')], let n = round (r / k), n < 1000 ] of
      (x : _) -> x
      [] -> (999, 'G')

-- | A number three figures wide, the zeros in front dimmed: "007". (With a
-- proportional font, add font_features='tnum' so all digits are equally
-- wide, and mind that its letters are not: an "M" is wider than a "K".)
figures :: Int -> String
figures n = dimZeros ++ show n
  where
    zeros = replicate (3 - length (show n)) '0'
    dimZeros = if null zeros then "" else "<span foreground='#928374'>" ++ zeros ++ "</span>"

-- | Hidden at the right edge: a column of icons to rebuild and restart
-- waymobar or rivermonad, log out, reboot and power off. With the pointer
-- on one, a popover beside it says what it does; logging out, rebooting
-- and powering off ask first.
sessionPanel :: SidePanel
sessionPanel = defaultSidePanel
  { panelRowHeight = 40   -- square buttons
  , panelWidgets =
      [ button "toolbar" "Rebuild waymobar" $
          onClick (\c -> when (clickButton c == LeftButton) restartWaymobar) . text
      , button "grid_view" "Rebuild rivermonad" $
          spawnOnClick "rivermonad --recompile && rivermonad --restart" . text
      , button "logout" "Log out" logoutButton
      , button "restart_alt" "Reboot" $
          confirmOnClick "Reboot" (spawn "systemctl reboot") . text
      , button "power_settings_new" "Power off" $
          confirmOnClick "Power off" (spawn "systemctl poweroff") . text
      ]
  }
  where
    -- an icon that says what it does while the pointer is on it
    button name tip widget = hoverMarkup (const tip) (widget (symbol name))

-- | A Material Symbols icon by its name ("wifi", "volume_up"...), in the
-- colour of the text around it, followed by a space.
sym :: String -> String
sym name = symbol name ++ " "

-- | The same without the space.
symbol :: String -> String
symbol = iconAt 14

-- | An icon at the size it has beside 10 pt text (14 is a good one in the
-- bar); it scales with 'fontSize'. An icon is bigger than the text, so it
-- is lowered to keep both centred on the line: with a bigger factor than
-- 330 the text sits higher beside the icon, with a smaller one lower.
--
-- The font is "Material Symbols Rounded", filled; there are also
-- "Outlined" and "Sharp", "@FILL=0" gives the outlined drawing and
-- "@wght=300" thinner strokes.
iconAt :: Double -> String -> String
iconAt size name =
  "<span font_desc='Material Symbols Rounded " ++ show points ++ " @FILL=1'"
    ++ " rise='" ++ show (negate (round (points * 330)) :: Int) ++ "'>" ++ name ++ "</span>"
  where points = size * fontSize / 10
