-- | The handful of cairo and pango functions waymobar draws with, bound
-- directly through the FFI (no gtk2hs).
module Waymobar.Draw
  ( Canvas
  , withCanvas
  , Color (..)
  , parseColor
  , setColor
  , fillRect
  , fillRounded
  , strokeRounded
  , fillRoundedReplace
  , fillEdgeTab
  , strokeEdgeTab
  , withEdgeTabClip
  , withFade
  , withFadeXY
  , withTranslate
  , withEdgeFade
  , clear
    -- * Text
  , Font
  , withFont
  , TextLayout
  , withMarkup
  , withMarkupBox
  , layoutSize
  , ellipsizeLayout
  , showLayout
  , validMarkup
  , escapeMarkup
    -- * Images
  , Icon (..)
  , drawIcon
  , canvasScale
  ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Char (isHexDigit, digitToInt)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Foreign.Ptr (castPtr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Data.Word (Word8)

data CairoSurface
data Cairo
data PangoLayout
data CairoPattern
data PangoFontDescription

-- | A cairo context drawing into a block of ARGB32 pixels.
newtype Canvas = Canvas (Ptr Cairo)

newtype Font = Font (Ptr PangoFontDescription)

newtype TextLayout = TextLayout (Ptr PangoLayout)

foreign import ccall unsafe "cairo_user_to_device_distance"
  c_user_to_device_distance :: Ptr Cairo -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall unsafe "cairo_image_surface_create_for_data"
  c_surface_for_data :: Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CairoSurface)
foreign import ccall unsafe "cairo_surface_destroy"
  c_surface_destroy :: Ptr CairoSurface -> IO ()
foreign import ccall unsafe "cairo_surface_flush"
  c_surface_flush :: Ptr CairoSurface -> IO ()
foreign import ccall unsafe "cairo_create"
  c_create :: Ptr CairoSurface -> IO (Ptr Cairo)
foreign import ccall unsafe "cairo_destroy"
  c_destroy :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_scale"
  c_scale :: Ptr Cairo -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_set_source_rgba"
  c_set_source_rgba :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_rectangle"
  c_rectangle :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_fill"
  c_fill :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_paint"
  c_paint :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_set_operator"
  c_set_operator :: Ptr Cairo -> CInt -> IO ()
foreign import ccall unsafe "cairo_move_to"
  c_move_to :: Ptr Cairo -> CDouble -> CDouble -> IO ()

foreign import ccall unsafe "cairo_new_path"
  c_new_path :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_close_path"
  c_close_path :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_arc"
  c_arc :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_arc_negative"
  c_arc_negative :: Ptr Cairo -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_line_to"
  c_line_to :: Ptr Cairo -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_set_line_width"
  c_set_line_width :: Ptr Cairo -> CDouble -> IO ()
foreign import ccall unsafe "cairo_stroke"
  c_stroke :: Ptr Cairo -> IO ()

foreign import ccall unsafe "cairo_save"
  c_save :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_restore"
  c_restore :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_translate"
  c_translate :: Ptr Cairo -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_set_source_surface"
  c_set_source_surface :: Ptr Cairo -> Ptr CairoSurface -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_get_source"
  c_get_source :: Ptr Cairo -> IO (Ptr ())
foreign import ccall unsafe "cairo_pattern_set_filter"
  c_pattern_set_filter :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "cairo_push_group"
  c_push_group :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_pop_group_to_source"
  c_pop_group_to_source :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_paint_with_alpha"
  c_paint_with_alpha :: Ptr Cairo -> CDouble -> IO ()

foreign import ccall unsafe "cairo_clip"
  c_clip :: Ptr Cairo -> IO ()
foreign import ccall unsafe "cairo_pattern_create_linear"
  c_pattern_create_linear :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Ptr CairoPattern)
foreign import ccall unsafe "cairo_pattern_add_color_stop_rgba"
  c_pattern_add_color_stop_rgba :: Ptr CairoPattern -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "cairo_pattern_destroy"
  c_pattern_destroy :: Ptr CairoPattern -> IO ()
foreign import ccall unsafe "cairo_mask"
  c_mask :: Ptr Cairo -> Ptr CairoPattern -> IO ()

foreign import ccall unsafe "pango_cairo_create_layout"
  c_create_layout :: Ptr Cairo -> IO (Ptr PangoLayout)
foreign import ccall unsafe "pango_cairo_show_layout"
  c_show_layout :: Ptr Cairo -> Ptr PangoLayout -> IO ()
foreign import ccall unsafe "g_object_unref"
  c_unref :: Ptr PangoLayout -> IO ()
foreign import ccall unsafe "pango_font_description_from_string"
  c_font_from_string :: CString -> IO (Ptr PangoFontDescription)
foreign import ccall unsafe "pango_font_description_free"
  c_font_free :: Ptr PangoFontDescription -> IO ()
foreign import ccall unsafe "pango_layout_set_font_description"
  c_set_font :: Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()
foreign import ccall unsafe "pango_layout_set_markup"
  c_set_markup :: Ptr PangoLayout -> CString -> CInt -> IO ()
foreign import ccall unsafe "waymobar_align_tabs"
  c_align_tabs :: Ptr PangoLayout -> CInt -> IO ()
foreign import ccall unsafe "pango_layout_set_width"
  c_set_width :: Ptr PangoLayout -> CInt -> IO ()
foreign import ccall unsafe "pango_layout_set_height"
  c_set_height :: Ptr PangoLayout -> CInt -> IO ()
foreign import ccall unsafe "pango_layout_set_wrap"
  c_set_wrap :: Ptr PangoLayout -> CInt -> IO ()
foreign import ccall unsafe "pango_layout_set_ellipsize"
  c_set_ellipsize :: Ptr PangoLayout -> CInt -> IO ()
foreign import ccall unsafe "pango_layout_set_alignment"
  c_set_alignment :: Ptr PangoLayout -> CInt -> IO ()
foreign import ccall unsafe "pango_layout_get_pixel_size"
  c_pixel_size :: Ptr PangoLayout -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "pango_parse_markup"
  c_parse_markup :: CString -> CInt -> CInt -> Ptr () -> Ptr () -> Ptr () -> Ptr (Ptr ()) -> IO CInt

-- | Draw into @w×h@ pixels (ARGB32, stride @4w@) at @ptr@. Drawing
-- coordinates are multiplied by @scale@.
withCanvas :: Ptr Word8 -> Int -> Int -> Double -> (Canvas -> IO a) -> IO a
withCanvas ptr w h scale act =
  bracket (c_surface_for_data ptr 0 (fi w) (fi h) (fi (4 * w))) (\s -> c_surface_flush s >> c_surface_destroy s) $ \s ->
    bracket (c_create s) c_destroy $ \cr -> do
      c_scale cr (realToFrac scale) (realToFrac scale)
      act (Canvas cr)

-- | RGBA, each in [0, 1].
data Color = Color !Double !Double !Double !Double
  deriving (Eq, Show)

-- | @#rgb@, @#rrggbb@ or @#rrggbbaa@.
parseColor :: String -> Maybe Color
parseColor ('#' : hex)
  | all isHexDigit hex = case hex of
      [r, g, b] -> Just (Color (one r) (one g) (one b) 1)
      [r1, r2, g1, g2, b1, b2] -> Just (Color (two r1 r2) (two g1 g2) (two b1 b2) 1)
      [r1, r2, g1, g2, b1, b2, a1, a2] -> Just (Color (two r1 r2) (two g1 g2) (two b1 b2) (two a1 a2))
      _ -> Nothing
  where
    one c = fromIntegral (digitToInt c * 17) / 255
    two a b = fromIntegral (digitToInt a * 16 + digitToInt b) / 255
parseColor _ = Nothing

setColor :: Canvas -> Color -> IO ()
setColor (Canvas cr) (Color r g b a) =
  c_set_source_rgba cr (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

fillRect :: Canvas -> Double -> Double -> Double -> Double -> IO ()
fillRect (Canvas cr) x y w h = do
  c_rectangle cr (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)
  c_fill cr

-- | Replace every pixel (alpha included) with the colour.
clear :: Canvas -> Color -> IO ()
clear cv@(Canvas cr) col = do
  c_set_operator cr 1 -- CAIRO_OPERATOR_SOURCE
  setColor cv col
  c_paint cr
  c_set_operator cr 2 -- CAIRO_OPERATOR_OVER

-- | A pango font description such as @"monospace 10"@ or @"Sans Bold 11"@.
withFont :: String -> (Font -> IO a) -> IO a
withFont desc act =
  bracket (B.useAsCString (utf8 desc) c_font_from_string) c_font_free (act . Font)

-- | A laid out piece of pango markup. Invalid markup is shown as text.
-- Tabs make columns: in text of several lines, what follows the first tab
-- of every line starts at the same place (and so on for the next tabs).
withMarkup :: Canvas -> Font -> String -> (TextLayout -> IO a) -> IO a
withMarkup (Canvas cr) (Font f) markup act =
  bracket (c_create_layout cr) c_unref $ \l -> do
    c_set_font l f
    ok <- validMarkup markup
    let m = if ok then markup else escapeMarkup markup
    B.useAsCString (utf8 m) $ \s -> c_set_markup l s (-1)
    c_align_tabs l 14
    act (TextLayout l)

-- | Like 'withMarkup', in a box @width@ pixels wide: centred, wrapped at
-- words, and cut with "…" after @lines@ lines.
withMarkupBox :: Canvas -> Font -> Double -> Int -> String -> (TextLayout -> IO a) -> IO a
withMarkupBox cv fnt width lines' markup act =
  withMarkup cv fnt markup $ \tl@(TextLayout l) -> do
    c_set_width l (round (width * 1024))      -- pango units
    c_set_height l (negate (fromIntegral (max 1 lines')))  -- negative: lines
    c_set_wrap l 0                            -- PANGO_WRAP_WORD
    c_set_ellipsize l 3                       -- PANGO_ELLIPSIZE_END
    c_set_alignment l 1                       -- PANGO_ALIGN_CENTER
    act tl

-- | Cut a layout to @width@ pixels, ending in "…" (one line).
ellipsizeLayout :: TextLayout -> Double -> IO ()
ellipsizeLayout (TextLayout l) width = do
  c_set_width l (round (max 0 width * 1024))   -- pango units
  c_set_ellipsize l 3                          -- PANGO_ELLIPSIZE_END

-- | Width and height in (logical) pixels.
layoutSize :: TextLayout -> IO (Int, Int)
layoutSize (TextLayout l) =
  alloca $ \pw -> alloca $ \ph -> do
    c_pixel_size l pw ph
    (,) <$> (fromIntegral <$> peek pw) <*> (fromIntegral <$> peek ph)

showLayout :: Canvas -> TextLayout -> Double -> Double -> IO ()
showLayout (Canvas cr) (TextLayout l) x y = do
  c_move_to cr (realToFrac x) (realToFrac y)
  c_show_layout cr l

validMarkup :: String -> IO Bool
validMarkup m =
  B.useAsCString (utf8 m) $ \s ->
    (/= 0) <$> c_parse_markup s (-1) 0 nullPtr nullPtr nullPtr nullPtr

-- | Make plain text safe to embed in pango markup.
escapeMarkup :: String -> String
escapeMarkup = concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '\'' -> "&#39;"
  '"' -> "&quot;"
  c -> [c]

utf8 :: String -> B.ByteString
utf8 = TE.encodeUtf8 . T.pack

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | An image: premultiplied ARGB32 in native byte order (cairo's format),
-- stride @4 * width@.
data Icon = Icon
  { iconWidth  :: !Int
  , iconHeight :: !Int
  , iconPixels :: !B.ByteString
  }

instance Eq Icon where
  a == b = iconWidth a == iconWidth b && iconHeight a == iconHeight b && iconPixels a == iconPixels b

instance Show Icon where
  show i = "<icon " ++ show (iconWidth i) ++ "x" ++ show (iconHeight i) ++ ">"

-- | Draw an icon scaled to fit a @size×size@ square at (x, y), centred.
drawIcon :: Canvas -> Icon -> Double -> Double -> Double -> IO ()
drawIcon (Canvas cr) (Icon w h px) x y size
  | w <= 0 || h <= 0 || B.length px < w * h * 4 = pure ()
  | otherwise = BU.unsafeUseAsCString px $ \p ->
      bracket (c_surface_for_data (castPtr p) 0 (fi w) (fi h) (fi (4 * w))) c_surface_destroy $ \s -> do
        let k = size / fromIntegral (max w h)
            dx = x + (size - k * fromIntegral w) / 2
            dy = y + (size - k * fromIntegral h) / 2
        c_save cr
        c_translate cr (realToFrac dx) (realToFrac dy)
        c_scale cr (realToFrac k) (realToFrac k)
        c_set_source_surface cr s 0 0
        pat <- c_get_source cr
        c_pattern_set_filter pat 1 -- CAIRO_FILTER_GOOD
        c_paint cr
        c_restore cr

-- | Device pixels per logical pixel of this canvas (the output scale).
canvasScale :: Canvas -> IO Double
canvasScale (Canvas cr) = alloca $ \px -> alloca $ \py -> do
  poke px 1
  poke py 0
  c_user_to_device_distance cr px py
  realToFrac <$> peek px

-- | A rectangle with rounded corners (radius clamped to half the shorter side).
roundedPath :: Ptr Cairo -> Double -> Double -> Double -> Double -> Double -> IO ()
roundedPath cr x y w h r0 = do
  let r = max 0 (minimum [r0, w / 2, h / 2])
      d = realToFrac :: Double -> CDouble
  c_new_path cr
  if r <= 0
    then c_rectangle cr (d x) (d y) (d w) (d h)
    else do
      c_arc cr (d (x + w - r)) (d (y + r)) (d r) (d (-pi / 2)) 0
      c_arc cr (d (x + w - r)) (d (y + h - r)) (d r) 0 (d (pi / 2))
      c_arc cr (d (x + r)) (d (y + h - r)) (d r) (d (pi / 2)) (d pi)
      c_arc cr (d (x + r)) (d (y + r)) (d r) (d pi) (d (3 * pi / 2))
      c_close_path cr

-- | The outline of a tab that grows out of a vertical edge of the screen:
-- a body from @y0@ to @y1@, @depth@ wide, starting at @edgeX@ and going
-- right (@dir = 1@) or left (@dir = -1@). Its far corners are rounded,
-- and where it meets the edge it flares out into it with inverted
-- corners, @r@ above and below the body, like a drop on a wall. Closed
-- along the edge if asked.
edgeTabPath :: Ptr Cairo -> Bool -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
edgeTabPath cr closed edgeX dir depth y0 y1 r0 = do
  let r = max 0 (minimum [r0, depth / 2, (y1 - y0) / 2])
      d = realToFrac :: Double -> CDouble
  c_save cr
  -- drawn as if the edge were at x = 0 and the body went right
  c_translate cr (d edgeX) 0
  c_scale cr (d (if dir < 0 then -1 else 1)) 1
  c_new_path cr
  c_move_to cr 0 (d (y0 - r))
  if r <= 0
    then do
      c_line_to cr 0 (d y0)
      c_line_to cr (d depth) (d y0)
      c_line_to cr (d depth) (d y1)
      c_line_to cr 0 (d y1)
    else do
      c_arc_negative cr (d r) (d (y0 - r)) (d r) (d pi) (d (pi / 2))
      c_arc cr (d (depth - r)) (d (y0 + r)) (d r) (d (-pi / 2)) 0
      c_arc cr (d (depth - r)) (d (y1 - r)) (d r) 0 (d (pi / 2))
      c_arc_negative cr (d r) (d (y1 + r)) (d r) (d (3 * pi / 2)) (d pi)
  when closed (c_close_path cr)
  -- the path keeps its place; only what is drawn later forgets the mirror
  c_restore cr

-- | Fill an edge tab with the current colour, replacing what is below it
-- (alpha included), like 'fillRoundedReplace'.
fillEdgeTab :: Canvas -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
fillEdgeTab (Canvas cr) edgeX dir depth y0 y1 r = do
  edgeTabPath cr True edgeX dir depth y0 y1 r
  c_set_operator cr 1 -- CAIRO_OPERATOR_SOURCE
  c_fill cr
  c_set_operator cr 2 -- CAIRO_OPERATOR_OVER

-- | Outline an edge tab (not along the screen's edge) with a line this wide.
strokeEdgeTab :: Canvas -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
strokeEdgeTab (Canvas cr) edgeX dir depth y0 y1 r lw = do
  edgeTabPath cr False edgeX dir depth y0 y1 r
  c_set_line_width cr (realToFrac lw)
  c_stroke cr

-- | Draw only inside an edge tab.
withEdgeTabClip :: Canvas -> Double -> Double -> Double -> Double -> Double -> Double -> IO a -> IO a
withEdgeTabClip (Canvas cr) edgeX dir depth y0 y1 r act = do
  c_save cr
  edgeTabPath cr True edgeX dir depth y0 y1 r
  c_clip cr
  x <- act
  c_restore cr
  pure x

-- | Fill a rounded rectangle with the current colour.
fillRounded :: Canvas -> Double -> Double -> Double -> Double -> Double -> IO ()
fillRounded (Canvas cr) x y w h r = roundedPath cr x y w h r >> c_fill cr

-- | Outline a rounded rectangle: the line lies inside the given box.
strokeRounded :: Canvas -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
strokeRounded (Canvas cr) x y w h r lw = do
  let i = lw / 2
  roundedPath cr (x + i) (y + i) (w - lw) (h - lw) (max 0 (r - i))
  c_set_line_width cr (realToFrac lw)
  c_stroke cr

-- | Like 'fillRounded', but the colour replaces what is below it (alpha
-- included) instead of being blended over it.
fillRoundedReplace :: Canvas -> Double -> Double -> Double -> Double -> Double -> IO ()
fillRoundedReplace (Canvas cr) x y w h r = do
  roundedPath cr x y w h r
  c_set_operator cr 1 -- CAIRO_OPERATOR_SOURCE
  c_fill cr
  c_set_operator cr 2 -- CAIRO_OPERATOR_OVER

-- | Draw everything in the action as one layer, moved @dy@ down and
-- blended with opacity @alpha@: one step of a fade (and slide) animation.
withFade :: Canvas -> Double -> Double -> IO a -> IO a
withFade cv alpha dy = withFadeXY cv alpha 0 dy

-- | Like 'withFade', moved @dx@ right too.
withFadeXY :: Canvas -> Double -> Double -> Double -> IO a -> IO a
withFadeXY (Canvas cr) alpha dx dy act = do
  c_push_group cr
  c_save cr
  c_translate cr (realToFrac dx) (realToFrac dy)
  r <- act
  c_restore cr
  c_pop_group_to_source cr
  c_paint_with_alpha cr (realToFrac (max 0 (min 1 alpha)))
  pure r

-- | Draw with the origin moved to @(dx, dy)@.
withTranslate :: Canvas -> Double -> Double -> IO a -> IO a
withTranslate (Canvas cr) dx dy act = do
  c_save cr
  c_translate cr (realToFrac dx) (realToFrac dy)
  r <- act
  c_restore cr
  pure r

-- | Draw only inside the rectangle @(x, y, w, h)@, its left @fadeL@ and
-- right @fadeR@ pixels fading out to transparent (the window of a ticker).
withEdgeFade :: Canvas -> Double -> Double -> Double -> Double -> Double -> Double -> IO a -> IO a
withEdgeFade (Canvas cr) x y w h fadeL fadeR act = do
  c_save cr
  c_new_path cr
  c_rectangle cr (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)
  c_clip cr
  c_push_group cr
  r <- act
  c_pop_group_to_source cr
  if (fadeL <= 0 && fadeR <= 0) || w <= 0 then c_paint cr else
    bracket (c_pattern_create_linear (realToFrac x) 0 (realToFrac (x + w)) 0) c_pattern_destroy $ \pat -> do
      let fl = realToFrac (max 0 (min 0.5 (fadeL / w)))
          fr = realToFrac (max 0 (min 0.5 (fadeR / w)))
          stop o a = c_pattern_add_color_stop_rgba pat o 0 0 0 a
      stop 0 (if fl > 0 then 0 else 1) >> stop fl 1 >> stop (1 - fr) 1 >> stop 1 (if fr > 0 then 0 else 1)
      c_mask cr pat
  c_restore cr
  pure r
