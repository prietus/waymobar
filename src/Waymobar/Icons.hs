-- | Icons: loading image files and looking names up in freedesktop icon
-- themes (simplified: the best file of that name in the theme or the
-- themes it inherits from, preferring scalable and large ones).
module Waymobar.Icons
  ( Icon (..)
  , loadIconFile
  , iconFromNetworkARGB
  , recolorIcon
  , lookupIcon
  , findIcon
  , resolveIcon
  , defaultIconThemes
  ) where

import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Char (isDigit)
import Data.List (isInfixOf, isSuffixOf, nub, sortOn, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Word (Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (alloca, finalizerFree)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

import Waymobar.Core (IconSource (..))
import Waymobar.Draw (Icon (..))

foreign import ccall safe "waymobar_load_icon"
  c_load_icon :: CString -> CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr Word8)

-- | Load an image file, scaled to fit in @size×size@ pixels.
loadIconFile :: FilePath -> Int -> IO (Maybe Icon)
loadIconFile path size =
  withCString path $ \cp -> alloca $ \pw -> alloca $ \ph -> do
    p <- c_load_icon cp (fromIntegral size) pw ph
    if p == nullPtr
      then pure Nothing
      else do
        w <- fromIntegral <$> peek pw
        h <- fromIntegral <$> peek ph
        fp <- newForeignPtr finalizerFree p
        pure (Just (Icon w h (BI.fromForeignPtr fp 0 (w * h * 4))))

-- | StatusNotifierItem pixmaps: ARGB32 in network byte order, not
-- premultiplied.
iconFromNetworkARGB :: Int -> Int -> B.ByteString -> Maybe Icon
iconFromNetworkARGB w h bs
  | w <= 0 || h <= 0 || B.length bs < w * h * 4 = Nothing
  | otherwise = Just (Icon w h (fst (B.unfoldrN (w * h * 4) step 0)))
  where
    -- output: B G R A (little-endian ARGB32), premultiplied
    step i =
      let px = i `div` 4
          k = i `mod` 4
          at j = fromIntegral (BU.unsafeIndex bs (px * 4 + j)) :: Int
          a = at 0
          pm v = fromIntegral (v * a `div` 255) :: Word8
          out = case k of
            0 -> pm (at 3)
            1 -> pm (at 2)
            2 -> pm (at 1)
            _ -> fromIntegral a
      in Just (out, i + 1)

-- | Paint every pixel with one colour, keeping its alpha (for the
-- monochrome @-symbolic@ icons, like GTK does).
recolorIcon :: (Double, Double, Double) -> Icon -> Icon
recolorIcon (r, g, b) (Icon w h px) = Icon w h (fst (B.unfoldrN (B.length px) step 0))
  where
    step i =
      let k = i `mod` 4
          a = fromIntegral (BU.unsafeIndex px (i - k + 3)) :: Double
          out = case k of
            0 -> b * a
            1 -> g * a
            2 -> r * a
            _ -> a
      in Just (round out :: Word8, i + 1)

-- | Themes searched when none is given: the GTK theme, then fallbacks.
defaultIconThemes :: IO [String]
defaultIconThemes = do
  gtk <- E.try @E.SomeException (readProcessWithExitCode "gsettings" ["get", "org.gnome.desktop.interface", "icon-theme"] "")
  let fromGsettings = case gtk of
        Right (_, out, _) | let t = filter (`notElem` "'\"\n") out, not (null t) -> [t]
        _ -> []
  ini <- E.try (readFile =<< (</> "gtk-3.0/settings.ini") <$> getXdgDirectory XdgConfig "")
  let fromIni = case ini of
        Right s -> [ dropWhile (== ' ') (drop 1 v) | l <- lines s, let (k, v) = break (== '=') l
                   , filter (/= ' ') k == "gtk-icon-theme-name" ]
        Left (_ :: E.IOException) -> []
  pure (nub (fromGsettings ++ fromIni ++ ["Adwaita", "hicolor"]))

-- | Find and load an icon by name (or absolute path), searching the extra
-- directories first (an item's IconThemePath). Like GTK, @foo@ falls back
-- to @foo-symbolic@ and the other way round. Symbolic icons are painted
-- with the given colour, if any.
lookupIcon :: Maybe (Double, Double, Double) -> [FilePath] -> [String] -> String -> Int -> IO (Maybe Icon)
lookupIcon symbolic extra themes name size = do
  let alt = case stripSuffix "-symbolic" name of
        Just base -> base
        Nothing -> name ++ "-symbolic"
  mp <- findIcon extra themes name >>= maybe (if isAbsolute name then pure Nothing else findIcon extra themes alt) (pure . Just)
  case mp of
    Nothing -> pure Nothing
    Just p -> do
      i <- loadIconFile p size
      pure $ case symbolic of
        Just col | "-symbolic" `isInfixOf` takeFileName p -> recolorIcon col <$> i
        _ -> i
  where
    stripSuffix suf str = reverse <$> stripPrefix (reverse suf) (reverse str)

findIcon :: [FilePath] -> [String] -> String -> IO (Maybe FilePath)
findIcon extra themes name
  | null name = pure Nothing
  | isAbsolute name = do
      e <- doesFileExist name
      pure (if e then Just name else Nothing)
  | otherwise = do
      fromExtra <- fmap concat $ forM extra $ \d -> do
        idx <- indexDir d
        pure (fromMaybe [] (M.lookup name idx))
      case best fromExtra of
        Just p -> pure (Just p)
        Nothing -> do
          chain <- themeChain themes
          found <- firstJustM chain $ \t -> do
            idx <- themeIndex t
            pure (best (fromMaybe [] (M.lookup name idx)))
          case found of
            Just p -> pure (Just p)
            Nothing -> do
              pix <- indexDir "/usr/share/pixmaps"
              pure (best (fromMaybe [] (M.lookup name pix)))
  where
    firstJustM [] _ = pure Nothing
    firstJustM (x : xs) f = f x >>= maybe (firstJustM xs f) (pure . Just)

-- | Prefer scalable images, then the largest bitmap (up to 256 pixels).
best :: [FilePath] -> Maybe FilePath
best = listToMaybe . sortOn (Down . score)
  where
    score p
      | ".svg" `isSuffixOf` p = if "scalable" `elem` dirs p then 10000 else size p + 300 :: Int
      | otherwise = size p
    dirs = splitDirectories
    size p = fromMaybe 1 (listToMaybe (mapMaybe sizeOf (dirs p)))
    sizeOf comp = case span isDigit comp of
      (n@(_ : _), 'x' : rest) | (m, sc) <- span isDigit rest, m == n ->
        let factor = case sc of
              '@' : f | all isDigit f, not (null f) -> read f
              _ -> 1
        in Just (min 256 (read n * factor))
      _ -> Nothing

------------------------------------------------------------------------------
-- Theme indexes (cached for the life of the process)

type Index = M.Map String [FilePath]

{-# NOINLINE indexCache #-}
indexCache :: MVar (M.Map FilePath Index)
indexCache = unsafePerformIO (newMVar M.empty)

themeDirs :: String -> IO [FilePath]
themeDirs theme = do
  home <- getHomeDirectory
  dataHome <- fromMaybe (home </> ".local/share") <$> lookupEnv "XDG_DATA_HOME"
  dataDirs <- maybe ["/usr/local/share", "/usr/share"] (splitSearchPath) <$> lookupEnv "XDG_DATA_DIRS"
  filterM doesDirectoryExist
    ((home </> ".icons" </> theme) : [ d </> "icons" </> theme | d <- dataHome : dataDirs ])

themeIndex :: String -> IO Index
themeIndex theme = do
  ds <- themeDirs theme
  M.unionsWith (++) <$> mapM indexDir ds

-- | Every image under a directory, by name without extension.
indexDir :: FilePath -> IO Index
indexDir dir = modifyMVar indexCache $ \cache -> case M.lookup dir cache of
  Just idx -> pure (cache, idx)
  Nothing -> do
    files <- walk dir
    let idx = M.fromListWith (++)
          [ (dropExtension (takeFileName f), [f]) | f <- files, takeExtension f `elem` [".png", ".svg", ".xpm"] ]
    pure (M.insert dir idx cache, idx)
  where
    walk d = do
      r <- E.try (listDirectory d)
      case r of
        Left (_ :: E.IOException) -> pure []
        Right es -> fmap concat $ forM es $ \e -> do
          let p = d </> e
          isDir <- doesDirectoryExist p
          if isDir then walk p else pure [p]

-- | The themes and everything they inherit, ending with hicolor.
themeChain :: [String] -> IO [String]
themeChain themes = (++ ["hicolor"]) . filter (/= "hicolor") . nub <$> go themes []
  where
    go [] seen = pure (reverse seen)
    go (t : ts) seen
      | t `elem` seen = go ts seen
      | otherwise = do
          parents <- inherits t
          go (parents ++ ts) (t : seen)
    inherits t = do
      ds <- themeDirs t
      fmap concat $ forM ds $ \d -> do
        r <- E.try (readFile (d </> "index.theme"))
        pure $ case r of
          Right s -> concat [ splitComma (drop 1 v) | l <- lines s, let (k, v) = break (== '=') l, k == "Inherits" ]
          Left (_ :: E.IOException) -> []
    splitComma s = case break (== ',') s of
      (a, _ : r) -> trim a : splitComma r
      (a, []) -> [trim a | not (null (trim a))]
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

------------------------------------------------------------------------------
-- Icons rasterized for drawing

type IconKey = (IconSource, Int, Maybe (Double, Double, Double), Bool)

iconCache :: MVar (M.Map IconKey (Maybe Icon))
iconCache = unsafePerformIO (newMVar M.empty)
{-# NOINLINE iconCache #-}

themesCache :: MVar (Maybe [String])
themesCache = unsafePerformIO (newMVar Nothing)
{-# NOINLINE themesCache #-}

-- | An icon rasterized at exactly @px@ device pixels (cached). Symbolic
-- theme icons are recoloured to @colour@; with @recolorAll@ every icon is.
resolveIcon :: IconSource -> Int -> Maybe (Double, Double, Double) -> Bool -> IO (Maybe Icon)
resolveIcon src px colour recolorAll = do
  let key = (src, px, colour, recolorAll)
  hit <- M.lookup key <$> readMVar iconCache
  case hit of
    Just r -> pure r
    Nothing -> do
      r <- load `E.catch` \(_ :: E.SomeException) -> pure Nothing
      -- Sizes change with the output scale; keep the cache from growing forever.
      modifyMVar_ iconCache $ \m -> pure (M.insert key r (if M.size m > 512 then M.empty else m))
      pure r
  where
    forAll i = case colour of
      Just c | recolorAll -> recolorIcon c i
      _ -> i
    load = case src of
      IconFile p -> do
        path <- expandHome p
        fmap forAll <$> loadIconFile path px
      IconName dirs name -> do
        themes <- modifyMVar themesCache $ \case
          Just ts -> pure (Just ts, ts)
          Nothing -> do
            ts <- defaultIconThemes
            pure (Just ts, ts)
        fmap forAll <$> lookupIcon (if recolorAll then Nothing else colour) dirs themes name px
    expandHome p = case stripPrefix "~/" p of
      Just rest -> (</> rest) <$> getHomeDirectory
      Nothing -> pure p
