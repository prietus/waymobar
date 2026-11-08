-- ~/.config/waymobar/waymobar.hs
--
-- waymobar rebuilds this file when it changes (like xmonad). Check it with
-- `waymobar --recompile`.
import Waymobar

main :: IO ()
main = waymobar def
  { font = "monospace 10"
  , position = Top
  , bgColor = "#1d2021"
  , fgColor = "#ebdbb2"
  , left =
      [ workspaces defaultWorkspaces
          { hideEmpty = True
          , focusedStyle = noStyle { styleFg = Just "#1d2021", styleBg = Just "#fabd2f" }
          , activeStyle = noStyle { styleUnderline = Just "#fabd2f" }
          , wsLabel = \w -> escape (wsName w) ++ (if wsWindows w > 1 then "<sup>" ++ show (wsWindows w) ++ "</sup>" else "")
          }
      , colored "#83a598" (layoutName (\l -> "[" ++ l ++ "]"))
      ]
  , center = [ windowTitle 70 ]
  , right =
      [ colored "#8ec07c" (cpu (\p -> "cpu " ++ show p ++ "%"))
      , colored "#d3869b" (memory (\p -> "mem " ++ show p ++ "%"))
      , command "uname -r" 3600
      , spawnOnClick "foot" (text "<b>+</b>")
      , colored "#fabd2f" (clock "%a %d %b  %H:%M:%S")
      ]
  }
