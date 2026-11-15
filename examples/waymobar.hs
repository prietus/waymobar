-- ~/.config/waymobar/waymobar.hs
--
-- waymobar rebuilds this file when it changes (like xmonad). Check it with
-- `waymobar --recompile`.
--
-- The icons are Material Symbols (https://fonts.google.com/icons), from
-- the variable font (ttf-material-symbols-variable on Arch). Without it
-- their names show as text: use plain labels instead ("cpu " ++ ...).
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
  }

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
