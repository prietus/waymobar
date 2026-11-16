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
      [ colored "#8ec07c" (cpu (\p -> sym "memory" ++ show p ++ "%"))
      , colored "#d3869b" (memory (\p -> sym "memory_alt" ++ show p ++ "%"))
      , command "uname -r" 3600
      , spawnOnClick "foot" (text (symbol "add"))
        -- 'clock' escapes its text, so the icon is added afterwards
      , colored "#fabd2f" (markup (sym "schedule" ++) (clock "%a %d %b  %H:%M:%S"))
      ]
    -- push the pointer against the right edge of the screen
  , sidePanel = Just sessionPanel
  }

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
