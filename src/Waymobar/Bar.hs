{-# LANGUAGE OverloadedStrings #-}
-- | The Wayland side: one layer-shell surface per output, drawn with
-- cairo/pango into shared memory buffers, plus popup menus.
--
-- Threads: a reader thread decodes compositor events, widgets update
-- their contents from their own threads; everything ends up in one queue
-- consumed by the main loop, which owns all Wayland state and redraws the
-- surfaces that changed once the queue is empty.
module Waymobar.Bar
  ( runBar
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.Binary.Get (runGetOrFail)
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (find, findIndex, mapAccumL)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import System.Environment (setEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (Fd (..))

import Waymobar.Core
import Waymobar.Draw
import Waymobar.Icons (resolveIcon)
import qualified Waymobar.Rivermonad as R
import Waymobar.Wayland.Protocol
import Waymobar.Wayland.Wire

foreign import ccall unsafe "waymobar_shm_file" c_shm_file :: CSize -> IO CInt
foreign import ccall unsafe "waymobar_mmap" c_mmap :: CInt -> CSize -> IO (Ptr Word8)
foreign import ccall unsafe "waymobar_munmap" c_munmap :: Ptr Word8 -> CSize -> IO ()

data Msg
  = MsgEvent Event
  | MsgRedraw
  | MsgClosed String
  | MsgMenu ObjectId (Double, Double) [MenuEntry]  -- ^ bar surface, anchor, entries
  | MsgMenuTimeout Int                              -- ^ close the menu if the pointer is still away

data Globals = Globals
  { gCompositor :: Maybe (ObjectId, Word32)  -- ^ object and bound version
  , gShm        :: Maybe ObjectId
  , gLayerShell :: Maybe (ObjectId, Word32)
  , gCursor     :: Maybe ObjectId
  , gFractional :: Maybe ObjectId
  , gViewporter :: Maybe ObjectId
  , gWmBase     :: Maybe ObjectId
  }

data Out = Out
  { oName   :: Maybe String
  , oScale  :: Int
  , oDone   :: Bool
  , oGlobal :: Word32
  , oBar    :: Maybe ObjectId   -- ^ the bar's wl_surface
  }

data Buffer = Buffer
  { bufObj  :: ObjectId
  , bufPtr  :: Ptr Word8
  , bufSize :: Int
  , bufW    :: Int
  , bufH    :: Int
  , bufBusy :: Bool
  }

-- | A wl_surface we draw into (a bar or a popup).
data Surf = Surf
  { sfSurface    :: ObjectId
  , sfFrac       :: Maybe ObjectId
  , sfViewport   :: Maybe ObjectId
  , sfOutput     :: ObjectId       -- ^ for the integer scale fallback
  , sfW          :: Int            -- ^ logical size
  , sfH          :: Int
  , sfConfigured :: Bool
  , sfDirty      :: Bool
  , sfScale120   :: Int            -- ^ fractional scale (120 = 1), 0 if unknown
  , sfBufScale   :: Int            -- ^ preferred integer buffer scale, 0 if unknown
  , sfBuffers    :: [Buffer]
  }

data Bar = Bar
  { bSurface    :: ObjectId
  , bLayer      :: ObjectId
  , bOutput     :: ObjectId
  , bOutputName :: String
  , bHits       :: [(Double, Double, Click -> IO ())]
  , bHoverX     :: Maybe Double                -- ^ pointer position, for hover styles
  , bHoverRects :: [(Double, Double)]          -- ^ blocks that have a hover style
  , bExpanded   :: Maybe Int                   -- ^ the block showing its hover markup (numbered left to right)
  }

-- | A row of an open menu.
data Row = RowEntry MenuEntry | RowBack

data Popup = Popup
  { pSurface :: ObjectId
  , pXdgSurf :: ObjectId
  , pPopup   :: ObjectId
  , pBar     :: ObjectId                -- ^ parent bar surface
  , pArea    :: (Double, Double)
  , pRows    :: [Row]
  , pLayout  :: [(Double, Double)]      -- ^ y range of every row
  , pStack   :: [[MenuEntry]]           -- ^ parent menus, for "back"
  , pHover   :: Maybe Int
  , pAway    :: Int                     -- ^ bumped whenever the pointer leaves our surfaces
  }

data Seat = Seat
  { sPointer :: Maybe ObjectId
  , sShape   :: Maybe ObjectId
  , sFocus   :: Maybe ObjectId  -- ^ our surface under the pointer
  , sX       :: Double
  , sY       :: Double
  , sAxis    :: Double          -- ^ accumulated vertical scroll
  }

data S = S
  { sGlobals   :: Globals
  , sOutputs   :: M.Map ObjectId Out
  , sSurfs     :: M.Map ObjectId Surf     -- ^ by wl_surface
  , sBars      :: M.Map ObjectId Bar      -- ^ by wl_surface
  , sPopup     :: Maybe Popup
  , sSeats     :: M.Map ObjectId Seat
  , sLastPress :: Maybe (ObjectId, Word32)  -- ^ seat and serial of the last button press
  , sReady     :: Bool                      -- ^ the initial roundtrip is done
  }

data Env = Env
  { eConn    :: Conn
  , eReg     :: ObjectId
  , eCfg     :: Config
  , eHeight  :: Int
  , eWidgets :: ([Output -> IO [Block]], [Output -> IO [Block]], [Output -> IO [Block]])
  , eState   :: IORef S
  , eSync    :: ObjectId
  , ePush    :: Msg -> IO ()
  }

-- | Run the bar until the compositor goes away.
runBar :: Config -> IO ()
runBar cfg = do
  -- gdk-pixbuf only reads local files: keep GIO from talking to GVfs
  setEnv "GIO_USE_VFS" "local"
  q <- newTQueueIO
  let push = atomically . writeTQueue q
  client <- R.startClient (push MsgRedraw)
  let benv = BarEnv
        { redraw = push MsgRedraw
        , rivermonadState = R.currentState client
        , rivermonadDo = R.sendAction
        }
  ws <- (,,) <$> mapM (`startWidget` benv) (left cfg)
             <*> mapM (`startWidget` benv) (center cfg)
             <*> mapM (`startWidget` benv) (right cfg)
  h <- if barHeight cfg > 0 then pure (barHeight cfg) else (+ 6) <$> fontHeight (font cfg)

  conn <- connect `E.catch` \(e :: E.SomeException) -> do
    hPutStrLn stderr ("waymobar: cannot connect to the Wayland compositor: " ++ show e)
    exitFailure
  reg <- wlDisplayGetRegistry conn displayObject
  sync <- wlDisplaySync conn displayObject
  flush conn
  _ <- forkIO (reader conn push)

  st <- newIORef S
    { sGlobals = Globals Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    , sOutputs = M.empty, sSurfs = M.empty, sBars = M.empty, sPopup = Nothing
    , sSeats = M.empty, sLastPress = Nothing, sReady = False }
  let env = Env conn reg cfg h ws st sync push
  let loop = do
        first <- atomically (readTQueue q)
        rest <- atomically (flushTQueue q)
        quit <- or <$> mapM (handle env) (first : rest)
        unless quit $ do
          redrawDirty env
          flush conn
          loop
  loop

-- | The height of one line of text in the font.
fontHeight :: String -> IO Int
fontHeight f =
  allocaBytes 4 $ \p ->
    withCanvas p 1 1 1 $ \cv -> withFont f $ \font' ->
      withMarkup cv font' "Ág" $ \l -> snd <$> layoutSize l

reader :: Conn -> (Msg -> IO ()) -> IO ()
reader conn push = do
  r <- E.try $ forever $ do
    msgs <- readMessages conn
    forM_ msgs $ \m -> do
      iface <- lookupInterface conn (rawSender m)
      case iface >>= \i -> decodeEvent i (rawSender m) (rawOpcode m) of
        Nothing -> pure ()
        Just g -> case runGetOrFail g (BL.fromStrict (rawBody m)) of
          Left _ -> hPutStrLn stderr ("waymobar: cannot decode event from " ++ show (rawSender m))
          Right (_, _, (ev, news)) -> do
            mapM_ (uncurry (registerObject conn)) news
            push (MsgEvent ev)
  case r of
    Left e -> push (MsgClosed (show (e :: E.SomeException)))
    Right () -> pure ()

------------------------------------------------------------------------------
-- Events

-- | Handle one message; True means quit.
handle :: Env -> Msg -> IO Bool
handle env = \case
  MsgClosed why -> do
    hPutStrLn stderr ("waymobar: " ++ why)
    pure True
  MsgRedraw -> do
    modifyIORef' (eState env) $ \s -> s { sSurfs = M.mapWithKey (\k sf -> if M.member k (sBars s) then sf { sfDirty = True } else sf) (sSurfs s) }
    pure False
  MsgMenu bar area entries -> openMenu env bar area entries [] >> pure False
  MsgMenuTimeout gen -> do
    -- river does not dismiss popups on outside clicks: close the menu once
    -- the pointer has been away from it (and from the bar) for a while
    s <- readIORef (eState env)
    let onUs = any (isJust . sFocus) (M.elems (sSeats s))
    forM_ (sPopup s) $ \p -> when (pAway p == gen && not onUs) (closeMenu env)
    pure False
  MsgEvent ev -> event env ev >> pure False

event :: Env -> Event -> IO ()
event env ev = case ev of
  EvWlDisplay _ (WlDisplayError obj code msg) -> do
    hPutStrLn stderr ("waymobar: protocol error on " ++ show obj ++ " (" ++ show code ++ "): " ++ BC.unpack msg)
    exitFailure
  EvWlRegistry _ (WlRegistryGlobal name iface ver) -> global env name iface ver
  EvWlRegistry _ (WlRegistryGlobalRemove name) -> do
    outs <- sOutputs <$> readIORef st
    forM_ [ o | (o, out) <- M.toList outs, oGlobal out == name ] $ \o -> do
      destroyBarOn env o
      modifyIORef' st $ \s -> s { sOutputs = M.delete o (sOutputs s) }
      wlOutputRelease c o
  EvWlCallback cb (WlCallbackDone _) | cb == eSync env -> do
    s <- readIORef st
    when (isNothing (gLayerShell (sGlobals s))) $ do
      hPutStrLn stderr "waymobar: the compositor does not support wlr-layer-shell"
      exitFailure
    when (isNothing (gCompositor (sGlobals s)) || isNothing (gShm (sGlobals s))) $ do
      hPutStrLn stderr "waymobar: the compositor lacks wl_compositor or wl_shm"
      exitFailure
    writeIORef st s { sReady = True }
    createBars env
  EvWlOutput o oe -> do
    let upd f = modifyIORef' st $ \s -> s { sOutputs = M.adjust f o (sOutputs s) }
    case oe of
      WlOutputName n -> upd (\out -> out { oName = Just (utf8 n) })
      WlOutputScale sc -> do
        upd (\out -> out { oScale = fromIntegral sc })
        modifyIORef' st $ \s -> s { sSurfs = M.map (\sf -> if sfOutput sf == o then sf { sfDirty = True } else sf) (sSurfs s) }
      WlOutputDone -> upd (\out -> out { oDone = True }) >> createBars env
      _ -> pure ()
  EvZwlrLayerSurfaceV1 ls lse -> do
    bars <- sBars <$> readIORef st
    forM_ (find ((== ls) . bLayer) (M.elems bars)) $ \b -> case lse of
      ZwlrLayerSurfaceV1Configure serial w h -> do
        zwlrLayerSurfaceV1AckConfigure c ls serial
        updSurf (bSurface b) $ \sf -> sf
          { sfW = if w == 0 then 1 else fromIntegral w
          , sfH = if h == 0 then eHeight env else fromIntegral h
          , sfConfigured = True
          , sfDirty = True }
      ZwlrLayerSurfaceV1Closed -> destroyBar env b
  EvXdgWmBase wm (XdgWmBasePing serial) -> xdgWmBasePong c wm serial
  EvXdgSurface xs (XdgSurfaceConfigure serial) -> do
    mp <- sPopup <$> readIORef st
    forM_ mp $ \p -> when (pXdgSurf p == xs) $ do
      xdgSurfaceAckConfigure c xs serial
      updSurf (pSurface p) (\sf -> sf { sfConfigured = True, sfDirty = True })
  EvXdgPopup xp XdgPopupPopupDone -> do
    mp <- sPopup <$> readIORef st
    forM_ mp $ \p -> when (pPopup p == xp) (closeMenu env)
  EvWpFractionalScaleV1 fs (WpFractionalScaleV1PreferredScale sc) ->
    modifyIORef' st $ \s -> s { sSurfs = M.map (\sf -> if sfFrac sf == Just fs then sf { sfScale120 = fromIntegral sc, sfDirty = True } else sf) (sSurfs s) }
  EvWlSurface surf (WlSurfacePreferredBufferScale sc) ->
    updSurf surf (\sf -> sf { sfBufScale = fromIntegral sc, sfDirty = True })
  EvWlBuffer buf WlBufferRelease ->
    modifyIORef' st $ \s -> s { sSurfs = M.map (\sf -> sf { sfBuffers = map (\x -> if bufObj x == buf then x { bufBusy = False } else x) (sfBuffers sf) }) (sSurfs s) }
  EvWlSeat seat (WlSeatCapabilities caps) -> seatCaps env seat caps
  EvWlPointer p pe -> pointer env p pe
  _ -> pure ()
  where
    c = eConn env
    st = eState env
    updSurf surf f = modifyIORef' st $ \s -> s { sSurfs = M.adjust f surf (sSurfs s) }

global :: Env -> Word32 -> B.ByteString -> Word32 -> IO ()
global env name iface ver = do
  let bind v = wlRegistryBind c (eReg env) name iface (min ver v)
      setG f = modifyIORef' st $ \s -> s { sGlobals = f (sGlobals s) }
  case iface of
    "wl_compositor" -> bind 6 >>= \o -> setG (\g -> g { gCompositor = Just (o, min ver 6) })
    "wl_shm" -> bind 1 >>= \o -> setG (\g -> g { gShm = Just o })
    "zwlr_layer_shell_v1" -> bind 4 >>= \o -> setG (\g -> g { gLayerShell = Just (o, min ver 4) })
    "wp_cursor_shape_manager_v1" -> bind 1 >>= \o -> setG (\g -> g { gCursor = Just o })
    "wp_fractional_scale_manager_v1" -> bind 1 >>= \o -> setG (\g -> g { gFractional = Just o })
    "wp_viewporter" -> bind 1 >>= \o -> setG (\g -> g { gViewporter = Just o })
    "xdg_wm_base" -> bind 2 >>= \o -> setG (\g -> g { gWmBase = Just o })
    "wl_output" -> do
      o <- bind 4
      modifyIORef' st $ \s -> s { sOutputs = M.insert o (Out Nothing 1 False name Nothing) (sOutputs s) }
    "wl_seat" -> do
      o <- bind 7
      modifyIORef' st $ \s -> s { sSeats = M.insert o (Seat Nothing Nothing Nothing 0 0 0) (sSeats s) }
    _ -> pure ()
  where
    c = eConn env
    st = eState env

------------------------------------------------------------------------------
-- Surfaces

-- | A new wl_surface with fractional scaling when available.
newSurf :: Env -> ObjectId -> IO (Maybe Surf)
newSurf env output = do
  g <- sGlobals <$> readIORef (eState env)
  forM (gCompositor g) $ \(comp, _) -> do
    surf <- wlCompositorCreateSurface c comp
    frac <- forM (gFractional g) $ \fm -> wpFractionalScaleManagerV1GetFractionalScale c fm surf
    vp <- case (frac, gViewporter g) of
      (Just _, Just vpm) -> Just <$> wpViewporterGetViewport c vpm surf
      _ -> pure Nothing
    pure (Surf surf frac vp output 1 1 False True 0 0 [])
  where c = eConn env

destroySurf :: Env -> ObjectId -> IO ()
destroySurf env surf = do
  s <- readIORef (eState env)
  forM_ (M.lookup surf (sSurfs s)) $ \sf -> do
    mapM_ (wpFractionalScaleV1Destroy c) (sfFrac sf)
    mapM_ (wpViewportDestroy c) (sfViewport sf)
    wlSurfaceDestroy c surf
    mapM_ (freeBuffer c) (sfBuffers sf)
  writeIORef (eState env) s
    { sSurfs = M.delete surf (sSurfs s)
    , sSeats = M.map (\se -> if sFocus se == Just surf then se { sFocus = Nothing } else se) (sSeats s) }
  where c = eConn env

freeBuffer :: Conn -> Buffer -> IO ()
freeBuffer c b = do
  wlBufferDestroy c (bufObj b)
  c_munmap (bufPtr b) (fromIntegral (bufSize b))

newBuffer :: Conn -> ObjectId -> Int -> Int -> IO (Maybe Buffer)
newBuffer c shm w h = do
  let stride = 4 * w
      size = stride * h
  fd <- c_shm_file (fromIntegral size)
  if fd < 0 then pure Nothing else do
    p <- c_mmap fd (fromIntegral size)
    if p == nullPtr then pure Nothing else do
      pool <- wlShmCreatePool c shm (Fd fd) (fromIntegral size)  -- closes fd once sent
      buf <- wlShmPoolCreateBuffer c pool 0 (fromIntegral w) (fromIntegral h) (fromIntegral stride) wlShmFormatArgb8888
      wlShmPoolDestroy c pool
      pure (Just (Buffer buf p size w h False))

redrawDirty :: Env -> IO ()
redrawDirty env = do
  s <- readIORef (eState env)
  forM_ (M.elems (sSurfs s)) $ \sf -> when (sfDirty sf && sfConfigured sf) $ do
    let surf = sfSurface sf
        w = fromIntegral (sfW sf)
        h = fromIntegral (sfH sf)
    case (M.lookup surf (sBars s), sPopup s) of
      (Just b, _) -> do
        res <- present env sf $ \cv -> paintBar env cv (bOutputName b) (bHoverX b) (bExpanded b) w h
        forM_ res $ \(hs, hr, ex) -> modifyIORef' (eState env) $ \s' -> s' { sBars = M.adjust (\b' -> b' { bHits = hs, bHoverRects = hr, bExpanded = ex }) surf (sBars s') }
      (_, Just p) | pSurface p == surf -> void $ present env sf $ \cv -> paintMenu env cv p w h
      _ -> pure ()

-- | Draw a surface into a free buffer and commit it. Nothing if no buffer
-- was free (it stays dirty and is drawn on release).
present :: Env -> Surf -> (Canvas -> IO a) -> IO (Maybe a)
present env sf draw = do
  s <- readIORef (eState env)
  case gShm (sGlobals s) of
    Nothing -> pure Nothing
    Just shm -> do
      let outScale = maybe 1 oScale (M.lookup (sfOutput sf) (sOutputs s))
          compVer = maybe 1 snd (gCompositor (sGlobals s))
          (scale, intScale) = case (sfScale120 sf, sfViewport sf) of
            (n, Just _) | n > 0 -> (fromIntegral n / 120, Nothing)
            _ -> let i = max 1 (if sfBufScale sf > 0 then sfBufScale sf else outScale) in (fromIntegral i, Just i)
          pw = max 1 (round (fromIntegral (sfW sf) * scale :: Double))
          ph = max 1 (round (fromIntegral (sfH sf) * scale :: Double))
          sized x = bufW x == pw && bufH x == ph
          (stale, keep) = foldr (\x (a, k) -> if not (bufBusy x) && not (sized x) then (x : a, k) else (a, x : k)) ([], []) (sfBuffers sf)
          surf = sfSurface sf
          setSf f = modifyIORef' (eState env) $ \s' -> s' { sSurfs = M.adjust f surf (sSurfs s') }
      mapM_ (freeBuffer c) stale
      mbuf <- case find (\x -> not (bufBusy x) && sized x) keep of
        Just x -> pure (Just (x, keep))
        Nothing
          | length keep >= 3 -> pure Nothing  -- wait for a release
          | otherwise -> fmap (\x -> (x, x : keep)) <$> newBuffer c shm pw ph
      case mbuf of
        Nothing -> setSf (\x -> x { sfBuffers = keep }) >> pure Nothing
        Just (buf, bufs) -> do
          r <- withCanvas (bufPtr buf) pw ph scale draw
          wlSurfaceAttach c surf (bufObj buf) 0 0
          case (intScale, sfViewport sf) of
            (Nothing, Just vp) -> do
              wlSurfaceSetBufferScale c surf 1
              wpViewportSetDestination c vp (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
            (Just i, _) -> when (compVer >= 3) $ wlSurfaceSetBufferScale c surf (fromIntegral i)
            _ -> pure ()
          if compVer >= 4
            then wlSurfaceDamageBuffer c surf 0 0 (fromIntegral pw) (fromIntegral ph)
            else wlSurfaceDamage c surf 0 0 (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
          wlSurfaceCommit c surf
          let bufs' = map (\x -> if bufObj x == bufObj buf then x { bufBusy = True } else x) bufs
          setSf (\x -> x { sfBuffers = bufs', sfDirty = False })
          pure (Just r)
  where c = eConn env

------------------------------------------------------------------------------
-- Bars

-- | Give a bar to every announced output that should have one.
createBars :: Env -> IO ()
createBars env = do
  s <- readIORef (eState env)
  when (sReady s) $
    forM_ (M.toList (sOutputs s)) $ \(o, out) ->
      when (oDone out && isNothing (oBar out) && wanted (oName out)) $ createBar env o out
  where
    wanted n = null (outputs (eCfg env)) || maybe False (`elem` outputs (eCfg env)) n

createBar :: Env -> ObjectId -> Out -> IO ()
createBar env o out = do
  g <- sGlobals <$> readIORef st
  msf <- newSurf env o
  forM_ ((,) <$> msf <*> gLayerShell g) $ \(sf, (shell, _)) -> do
    let surf = sfSurface sf
        cfg = eCfg env
        lyr = case layer cfg of
          BackgroundLayer -> zwlrLayerShellV1LayerBackground
          BottomLayer -> zwlrLayerShellV1LayerBottom
          TopLayer -> zwlrLayerShellV1LayerTop
          OverlayLayer -> zwlrLayerShellV1LayerOverlay
        edge = if position cfg == Top then zwlrLayerSurfaceV1AnchorTop else zwlrLayerSurfaceV1AnchorBottom
    ls <- zwlrLayerShellV1GetLayerSurface c shell surf o lyr (encodeUtf8 (namespace cfg))
    zwlrLayerSurfaceV1SetSize c ls 0 (fromIntegral (eHeight env))
    zwlrLayerSurfaceV1SetAnchor c ls (edge .|. zwlrLayerSurfaceV1AnchorLeft .|. zwlrLayerSurfaceV1AnchorRight)
    -- The compositor adds the margin of the anchored edge to the exclusive
    -- zone, so windows still start right below (or above) the bar.
    let (mt, mr, mb, ml) = margins cfg
    zwlrLayerSurfaceV1SetMargin c ls (fromIntegral mt) (fromIntegral mr) (fromIntegral mb) (fromIntegral ml)
    zwlrLayerSurfaceV1SetExclusiveZone c ls (if exclusive cfg then fromIntegral (eHeight env) else 0)
    wlSurfaceCommit c surf
    modifyIORef' st $ \s -> s
      { sSurfs = M.insert surf sf { sfH = eHeight env } (sSurfs s)
      , sBars = M.insert surf (Bar surf ls o (fromMaybe "" (oName out)) [] Nothing [] Nothing) (sBars s)
      , sOutputs = M.adjust (\x -> x { oBar = Just surf }) o (sOutputs s) }
  where
    c = eConn env
    st = eState env

destroyBarOn :: Env -> ObjectId -> IO ()
destroyBarOn env o = do
  bars <- sBars <$> readIORef (eState env)
  mapM_ (destroyBar env) [ b | b <- M.elems bars, bOutput b == o ]

destroyBar :: Env -> Bar -> IO ()
destroyBar env b = do
  mp <- sPopup <$> readIORef (eState env)
  forM_ mp $ \p -> when (pBar p == bSurface b) (closeMenu env)
  zwlrLayerSurfaceV1Destroy (eConn env) (bLayer b)
  destroySurf env (bSurface b)
  modifyIORef' (eState env) $ \s -> s
    { sBars = M.delete (bSurface b) (sBars s)
    , sOutputs = M.adjust (\x -> x { oBar = Nothing }) (bOutput b) (sOutputs s) }

------------------------------------------------------------------------------
-- Drawing bars

-- | Draw a bar; returns its click areas, the areas that react to the
-- pointer, and the block showing its hover markup, if any.
paintBar :: Env -> Canvas -> Output -> Maybe Double -> Maybe Int -> Double -> Double
         -> IO ([(Double, Double, Click -> IO ())], [(Double, Double)], Maybe Int)
paintBar env cv out hoverX expanded w h = do
  let cfg = eCfg env
      (ls, cs, rs) = eWidgets env
      hasIcon b = isJust (blockIcon b) || isJust (blockIconSource b)
      visible b = not (null (blockMarkup b)) || hasIcon b
      -- widgets with nothing to show take no room (nor spacing)
      blocksOf = fmap (filter (not . null)) . mapM (\f -> filter visible <$> safe (f out))
  -- the bar itself: transparent outside its (possibly rounded) box
  clear cv (Color 0 0 0 0)
  setColor cv (color (bgColor cfg))
  fillRounded cv 0 0 w h (barRadius cfg)
  forM_ (barBorder cfg) $ \(bw, bc) -> setColor cv (color bc) >> strokeRounded cv 0 0 w h (barRadius cfg) bw
  devScale <- canvasScale cv
  let -- Icons with a source are rasterized at the real (device) size, so
      -- SVGs stay sharp at any output scale; the bitmap is a fallback.
      blockBitmap b st isz = case blockIconSource b of
        Nothing -> pure (blockIcon b)
        Just src -> do
          let px = max 1 (round (isz * devScale))
              Color r g bl _ = color (fromMaybe (fromMaybe (fgColor cfg) (styleFg st)) (blockIconColor b))
          ic <- resolveIcon src px (Just (r, g, bl)) (blockIconRecolorAll b)
          pure (maybe (blockIcon b) Just ic)
  lb0 <- blocksOf ls
  cb0 <- blocksOf cs
  rb0 <- blocksOf rs
  let allBlocks = concat (lb0 ++ cb0 ++ rb0)
      hasHover i = maybe False (isJust . blockHoverMarkup) (listToMaybe (drop i allBlocks))
      -- block number @mi@ (left to right) shows its hover markup
      expand mi (a, b, c) = let (i1, a') = area 0 a; (i2, b') = area i1 b; (_, c') = area i2 c in (a', b', c')
        where
          area = mapAccumL (mapAccumL swap)
          swap i blk = (i + 1 :: Int, if Just i == mi then blk { blockMarkup = fromMaybe (blockMarkup blk) (blockHoverMarkup blk) } else blk)
  withFont (font cfg) $ \fnt -> do
    let iconSize b = fromIntegral (if blockIconSize b > 0 then blockIconSize b else max 8 (round h - 8))
        iconPart b
          | hasIcon b = iconSize b + (if null (blockMarkup b) then 0 else 4)
          | otherwise = 0
        measure bss k = withLayouts cv fnt (map blockMarkup (concat bss)) $ \lays -> do
          ws <- forM (zip (concat bss) lays) $ \(b, l) -> do
            (tw, th) <- layoutSize l
            let st = blockStyle b
                (_, mh) = boxMargin st
                textW = if null (blockMarkup b) then 0 else fromIntegral tw
                inner = iconPart b + textW + 2 * (fromIntegral (blockPadding b) + boxPadding st)
            pure (max (fromIntegral (blockMinWidth b)) inner + 2 * mh, th)
          k (regroup bss (zip3 (concat bss) lays ws))
        sp = fromIntegral (widgetSpacing cfg)
        pad = fromIntegral (edgePadding cfg)
        groupStyleOf g = case g of
          ((b, _, _) : _) -> blockGroupStyle b
          [] -> Nothing
        -- room taken by a widget's own box around its blocks, at each side
        groupInset g = maybe 0 (\gs -> snd (boxMargin gs) + boxPadding gs) (groupStyleOf g)
        groupWidth g = sum [ bw | (_, _, (bw, _)) <- g ] + 2 * groupInset g
        totalWidth gs = sum (map groupWidth gs) + sp * fromIntegral (max 0 (length gs - 1))
        hovered x bw = maybe False (\hx -> hx >= x && hx < x + bw) hoverX
        drawGroups x0 gs = fmap unzip' $ forM (zip (scanl (\x g -> x + groupWidth g + sp) x0 gs) gs) $ \(gx, g) -> do
          forM_ (groupStyleOf g) $ \gs -> do
            let gst = if hovered gx (groupWidth g) then maybe gs (`mergeStyle` gs) (styleHover gs) else gs
            drawBox cv gst gx (groupWidth g) h
          fmap unzip' $ forM (zip (scanl (+) (gx + groupInset g) [ bw | (_, _, (bw, _)) <- g ]) g) $ \(x, (b, l, (bw, th))) -> do
            let base = blockStyle b
                st = if hovered x bw then maybe base (`mergeStyle` base) (styleHover base) else base
            ic <- blockBitmap b st (iconSize b)
            drawBlock cfg cv b { blockIcon = ic, blockStyle = st } l x bw h th (iconSize b)
            pure ( [ (x, x + bw, f) | Just f <- [blockOnClick b] ]
                 , [ (x, x + bw) | isJust (styleHover base) || isJust (blockHoverMarkup b) ] )
        unzip' xs = (concatMap fst xs, concatMap snd xs)
        -- the slot of every block, in numbering order
        blockRects x0 gs = concat
          [ [ (x, bw) | (x, (_, _, (bw, _))) <- zip (scanl (+) (gx + groupInset g) [ bw | (_, _, (bw, _)) <- g ]) g ]
          | (gx, g) <- zip (scanl (\x g -> x + groupWidth g + sp) x0 gs) gs ]
        layout mi k = let (lb, cb, rb) = expand mi (lb0, cb0, rb0) in
          measure lb $ \lgs -> measure cb $ \cgs -> measure rb $ \rgs -> do
            let lw = totalWidth lgs
                rw = totalWidth rgs
                cw = totalWidth cgs
                rx = w - pad - rw
                cx = max (pad + lw + sp) (min (rx - sp - cw) ((w - cw) / 2))
            k (lgs, cgs, rgs, cx, rx)
        hoveredBlock (lgs, cgs, rgs, cx, rx) =
          findIndex (uncurry hovered) (blockRects pad lgs ++ blockRects cx cgs ++ blockRects rx rgs)
        draw ex (lgs, cgs, rgs, cx, rx) = do
          (h1, r1) <- drawGroups pad lgs
          (h2, r2) <- drawGroups cx cgs
          (h3, r3) <- drawGroups rx rgs
          -- hover areas also cover the boxes of whole widgets
          let groupRects x0 gs = [ (gx, gx + groupWidth g)
                                 | (gx, g) <- zip (scanl (\x g -> x + groupWidth g + sp) x0 gs) gs
                                 , Just gs' <- [groupStyleOf g], isJust (styleHover gs') ]
          pure (h1 ++ h2 ++ h3, r1 ++ r2 ++ r3 ++ groupRects pad lgs ++ groupRects cx cgs ++ groupRects rx rgs, ex)
    -- The pointer is tested against what is on screen (with the block
    -- expanded last time); if that changes which block shows its hover
    -- markup, the bar is laid out again before drawing. An expanded block
    -- keeps the pointer inside it (it grows towards the free space), so
    -- there is no flicker at its edges.
    layout expanded $ \g -> do
      let ex = case hoveredBlock g of
            Just i | hasHover i -> Just i
            _ -> Nothing
      if ex == expanded then draw ex g else layout ex (draw ex)
  where
    safe act = act `E.catch` \(e :: E.SomeException) -> do
      hPutStrLn stderr ("waymobar: widget failed: " ++ show e)
      pure []
    regroup [] _ = []
    regroup (g : gs) xs = let (a, b) = splitAt (length g) xs in a : regroup gs b

boxMargin :: Style -> (Double, Double)
boxMargin = fromMaybe (0, 0) . styleMargin

boxPadding :: Style -> Double
boxPadding = fromMaybe 0 . stylePadding

-- | The box of a style (background, lines, border) in a slot of the bar.
drawBox :: Canvas -> Style -> Double -> Double -> Double -> IO ()
drawBox cv st x bw h = do
  let (mv, mh) = boxMargin st
      bx = x + mh
      by = mv
      bwid = bw - 2 * mh
      bh = h - 2 * mv
      r = fromMaybe 0 (styleRadius st)
      line = 2
  when (bwid > 0 && bh > 0) $ do
    forM_ (styleBg st) $ \bg -> setColor cv (color bg) >> fillRounded cv bx by bwid bh r
    forM_ (styleUnderline st) $ \u -> setColor cv (color u) >> fillRect cv (bx + r / 2) (by + bh - line) (bwid - r) line
    forM_ (styleOverline st) $ \u -> setColor cv (color u) >> fillRect cv (bx + r / 2) by (bwid - r) line
    forM_ (styleBorder st) $ \(lw, bc) -> setColor cv (color bc) >> strokeRounded cv bx by bwid bh r lw

-- | Lay out several pieces of markup; continuation-passing, so the layouts
-- stay alive while drawing.
withLayouts :: Canvas -> Font -> [String] -> ([TextLayout] -> IO a) -> IO a
withLayouts _ _ [] k = k []
withLayouts cv fnt (m : ms) k = withMarkup cv fnt m $ \l -> withLayouts cv fnt ms (\ls' -> k (l : ls'))

color :: String -> Color
color s = fromMaybe (Color 0 0 0 1) (parseColor s)

drawBlock :: Config -> Canvas -> Block -> TextLayout -> Double -> Double -> Double -> Int -> Double -> IO ()
drawBlock cfg cv b l x bw h th isz = do
  let st = blockStyle b
      (_, mh) = boxMargin st
      pad = fromIntegral (blockPadding b) + boxPadding st
  drawBox cv st x bw h
  tx <- case blockIcon b of
    Nothing -> pure (x + mh + pad)
    Just icon -> do
      drawIcon cv icon (x + mh + pad) (fromIntegral (round ((h - isz) / 2) :: Int)) isz
      pure (x + mh + pad + isz + 4)
  unless (null (blockMarkup b)) $ do
    setColor cv (color (fromMaybe (fgColor cfg) (styleFg st)))
    showLayout cv l tx (fromIntegral (round ((h - fromIntegral th) / 2) :: Int))

------------------------------------------------------------------------------
-- Menus

menuPad, menuBorderW :: Double
menuPad = 4
menuBorderW = 1

rowsFor :: [MenuEntry] -> [[MenuEntry]] -> [Row]
rowsFor entries stack = (if null stack then [] else [RowBack]) ++ map RowEntry entries

rowLabel :: Row -> String
rowLabel = \case
  RowBack -> "‹  back"
  RowEntry (MenuItem l _ _ _ _) -> l
  RowEntry (Submenu l _) -> l
  RowEntry MenuSeparator -> ""

isSeparator :: Row -> Bool
isSeparator = \case
  RowEntry MenuSeparator -> True
  _ -> False

-- | Size of a menu and the y range of every row.
measureMenu :: Config -> [Row] -> IO (Int, Int, [(Double, Double)])
measureMenu cfg rows =
  allocaBytes 4 $ \p -> withCanvas p 1 1 1 $ \cv -> withFont (font cfg) $ \fnt ->
    withLayouts cv fnt (map (escapeMarkup . rowLabel) rows) $ \lays -> do
      sizes <- mapM layoutSize lays
      let lineH = fromIntegral (maximum (16 : map snd sizes)) + 8
          heights = [ if isSeparator r then 9 else lineH | r <- rows ]
          ys = scanl (+) (menuBorderW + menuPad) heights
          textW = fromIntegral (maximum (0 : map fst sizes))
          w = 2 * menuBorderW + leftGutter rows + textW + 32
          h = last ys + menuPad + menuBorderW
      pure (ceiling w, ceiling h, zip ys (drop 1 ys))

leftGutter :: [Row] -> Double
leftGutter rows = 10 + (if any hasMark rows then 18 else 0) + (if any hasIcon rows then 22 else 0)
  where
    hasMark = \case
      RowEntry MenuItem { itemToggle = Just _ } -> True
      _ -> False
    hasIcon = \case
      RowEntry MenuItem { itemIcon = Just _ } -> True
      _ -> False

openMenu :: Env -> ObjectId -> (Double, Double) -> [MenuEntry] -> [[MenuEntry]] -> IO ()
openMenu env barSurf area entries stack = do
  closeMenu env
  s <- readIORef st
  let g = sGlobals s
      cfg = eCfg env
      rows = rowsFor entries stack
  case (gWmBase g, M.lookup barSurf (sBars s), sLastPress s) of
    (Just wm, Just bar, Just (seat, serial)) | not (null rows) -> do
      (w, h, ys) <- measureMenu cfg rows
      msf <- newSurf env (bOutput bar)
      forM_ msf $ \sf -> do
        let surf = sfSurface sf
            (x0, x1) = area
            barH = maybe (eHeight env) sfH (M.lookup barSurf (sSurfs s))
        xs <- xdgWmBaseGetXdgSurface c wm surf
        pos <- xdgWmBaseCreatePositioner c wm
        xdgPositionerSetSize c pos (fromIntegral w) (fromIntegral h)
        xdgPositionerSetAnchorRect c pos (round x0) 0 (max 1 (round (x1 - x0))) (fromIntegral barH)
        if position cfg == Top
          then do
            xdgPositionerSetAnchor c pos xdgPositionerAnchorBottomLeft
            xdgPositionerSetGravity c pos xdgPositionerGravityBottomRight
          else do
            xdgPositionerSetAnchor c pos xdgPositionerAnchorTopLeft
            xdgPositionerSetGravity c pos xdgPositionerGravityTopRight
        xdgPositionerSetConstraintAdjustment c pos
          (xdgPositionerConstraintAdjustmentSlideX .|. xdgPositionerConstraintAdjustmentSlideY .|. xdgPositionerConstraintAdjustmentFlipY)
        popup <- xdgSurfaceGetPopup c xs nullObject pos
        xdgPositionerDestroy c pos
        zwlrLayerSurfaceV1GetPopup c (bLayer bar) popup
        xdgPopupGrab c popup seat serial
        wlSurfaceCommit c surf
        modifyIORef' st $ \s' -> s'
          { sSurfs = M.insert surf sf { sfW = w, sfH = h } (sSurfs s')
          , sPopup = Just (Popup surf xs popup barSurf area rows ys stack Nothing 0) }
    _ -> pure ()
  where
    c = eConn env
    st = eState env

closeMenu :: Env -> IO ()
closeMenu env = do
  mp <- sPopup <$> readIORef (eState env)
  forM_ mp $ \p -> do
    xdgPopupDestroy (eConn env) (pPopup p)
    xdgSurfaceDestroy (eConn env) (pXdgSurf p)
    destroySurf env (pSurface p)
    modifyIORef' (eState env) $ \s -> s { sPopup = Nothing }

paintMenu :: Env -> Canvas -> Popup -> Double -> Double -> IO ()
paintMenu env cv p w h = do
  let cfg = eCfg env
      fg = color (fgColor cfg)
      dim = let Color r g b a = fg in Color r g b (a * 0.4)
      gutter = leftGutter (pRows p)
      markX = 10
      iconX = if any isMark (pRows p) then 28 else 10
  clear cv (color (menuBorder cfg))
  setColor cv (color (bgColor cfg))
  fillRect cv menuBorderW menuBorderW (w - 2 * menuBorderW) (h - 2 * menuBorderW)
  withFont (font cfg) $ \fnt ->
    withLayouts cv fnt (map (escapeMarkup . rowLabel) (pRows p)) $ \lays ->
      forM_ (zip3 [0 ..] (pRows p) (zip lays (pLayout p))) $ \(i, row, (l, (y0, y1))) ->
        if isSeparator row
          then do
            setColor cv (color (menuBorder cfg))
            fillRect cv (menuBorderW + 6) (fromIntegral (round ((y0 + y1) / 2) :: Int)) (w - 2 * menuBorderW - 12) 1
          else do
            let enabled = rowEnabled row
            when (pHover p == Just i && enabled) $ do
              setColor cv (color (menuHighlight cfg))
              fillRect cv menuBorderW y0 (w - 2 * menuBorderW) (y1 - y0)
            (_, th) <- layoutSize l
            let ty = fromIntegral (round (y0 + (y1 - y0 - fromIntegral th) / 2) :: Int)
            setColor cv (if enabled then fg else dim)
            case row of
              RowEntry MenuItem { itemToggle = Just True } ->
                withMarkup cv fnt "✓" $ \m -> showLayout cv m markX ty
              _ -> pure ()
            case row of
              RowEntry MenuItem { itemIcon = Just icon } ->
                drawIcon cv icon iconX (y0 + (y1 - y0 - 16) / 2) 16
              _ -> pure ()
            setColor cv (if enabled then fg else dim)
            showLayout cv l gutter ty
            case row of
              RowEntry (Submenu _ _) -> withMarkup cv fnt "›" $ \m -> showLayout cv m (w - 18) ty
              _ -> pure ()
  where
    isMark = \case
      RowEntry MenuItem { itemToggle = Just _ } -> True
      _ -> False

rowEnabled :: Row -> Bool
rowEnabled = \case
  RowEntry MenuItem { itemEnabled = e } -> e
  RowEntry (Submenu _ es) -> not (null es)
  RowEntry MenuSeparator -> False
  RowBack -> True

-- | A click on a menu row.
activateRow :: Env -> Popup -> Int -> IO ()
activateRow env p i = case drop i (pRows p) of
  (row : _) | rowEnabled row -> case row of
    RowEntry MenuItem { itemAction = act } -> do
      closeMenu env
      void $ forkIO $ void $ E.try @E.SomeException act
    RowEntry (Submenu _ es) ->
      openMenu env (pBar p) (pArea p) es (currentEntries : pStack p)
    RowBack -> case pStack p of
      (parent : rest) -> openMenu env (pBar p) (pArea p) parent rest
      [] -> pure ()
    RowEntry MenuSeparator -> pure ()
  _ -> pure ()
  where
    currentEntries = [ e | RowEntry e <- pRows p ]

------------------------------------------------------------------------------
-- Pointer

seatCaps :: Env -> ObjectId -> Word32 -> IO ()
seatCaps env seat caps = do
  s <- readIORef st
  forM_ (M.lookup seat (sSeats s)) $ \se -> do
    let hasPtr = (caps `div` wlSeatCapabilityPointer) `mod` 2 == 1
    case (hasPtr, sPointer se) of
      (True, Nothing) -> do
        p <- wlSeatGetPointer c seat
        shape <- forM (gCursor (sGlobals s)) $ \m -> wpCursorShapeManagerV1GetPointer c m p
        setSeat se { sPointer = Just p, sShape = shape }
      (False, Just p) -> do
        mapM_ (wpCursorShapeDeviceV1Destroy c) (sShape se)
        wlPointerRelease c p
        setSeat se { sPointer = Nothing, sShape = Nothing, sFocus = Nothing }
      _ -> pure ()
  where
    c = eConn env
    st = eState env
    setSeat se = modifyIORef' st $ \s -> s { sSeats = M.insert seat se (sSeats s) }

pointer :: Env -> ObjectId -> WlPointerEvent -> IO ()
pointer env p pe = do
  s <- readIORef st
  forM_ [ (seat, se) | (seat, se) <- M.toList (sSeats s), sPointer se == Just p ] $ \(seat, se) -> do
    let setSeat se' = modifyIORef' st $ \s' -> s' { sSeats = M.insert seat se' (sSeats s') }
    case pe of
      WlPointerEnter serial surf x y -> do
        forM_ (sShape se) $ \d -> wpCursorShapeDeviceV1SetShape c d serial wpCursorShapeDeviceV1ShapeDefault
        setSeat se { sFocus = if M.member surf (sSurfs s) then Just surf else Nothing, sX = x, sY = y, sAxis = 0 }
        hover (Just surf) x y
      WlPointerLeave _ _ -> do
        setSeat se { sFocus = Nothing, sAxis = 0 }
        hover Nothing 0 0
        s' <- readIORef st
        forM_ (sPopup s') $ \pop -> do
          let gen = pAway pop + 1
          modifyIORef' st $ \x -> x { sPopup = Just pop { pAway = gen } }
          void $ forkIO $ threadDelay 700000 >> ePush env (MsgMenuTimeout gen)
      WlPointerMotion _ x y -> do
        setSeat se { sX = x, sY = y }
        hover (sFocus se) x y
      WlPointerButton serial _ btn state | state == wlPointerButtonStatePressed -> do
        modifyIORef' st $ \s' -> s' { sLastPress = Just (seat, serial) }
        case sPopup s of
          Just pop | sFocus se == Just (pSurface pop) ->
            forM_ (rowAt pop (sY se)) (activateRow env pop)
          Just pop -> do
            -- a click on the bar closes the menu; on another block it also
            -- does what it normally does
            closeMenu env
            let (a0, a1) = pArea pop
                onOpener = sFocus se == Just (pBar pop) && sX se >= a0 && sX se < a1
            unless onOpener $ click se (button btn)
          Nothing -> click se (button btn)
      _ -> pointerRest se setSeat
  where
    button :: Word32 -> Button
    button = \case
      0x110 -> LeftButton
      0x111 -> RightButton
      0x112 -> MiddleButton
      n -> OtherButton (fromIntegral n)
    pointerRest se setSeat = case pe of
      WlPointerAxis _ axis v | axis == wlPointerAxisVerticalScroll -> do
        let acc = sAxis se + v
        if abs acc >= 10
          then do
            setSeat se { sAxis = 0 }
            click se (if acc > 0 then ScrollDown else ScrollUp)
          else setSeat se { sAxis = acc }
      WlPointerAxisStop _ axis | axis == wlPointerAxisVerticalScroll -> setSeat se { sAxis = 0 }
      _ -> pure ()
    c = eConn env
    st = eState env
    rowAt pop y = findIndex (\(y0, y1) -> y >= y0 && y < y1) (pLayout pop)
    hover :: Maybe ObjectId -> Double -> Double -> IO ()
    hover focus x y = do
      s <- readIORef st
      -- bars: redraw when the pointer moves onto another hover area
      forM_ (M.elems (sBars s)) $ \b -> do
        let hx = if focus == Just (bSurface b) then Just x else Nothing
            under = \px -> [ r | r@(x0, x1) <- bHoverRects b, Just p' <- [px], p' >= x0, p' < x1 ]
            changed = under hx /= under (bHoverX b)
        when (hx /= bHoverX b) $ modifyIORef' st $ \s' -> s'
          { sBars = M.adjust (\b' -> b' { bHoverX = hx }) (bSurface b) (sBars s')
          , sSurfs = if changed then M.adjust (\sf -> sf { sfDirty = True }) (bSurface b) (sSurfs s') else sSurfs s' }
      forM_ (sPopup s) $ \pop -> do
        let h = if focus == Just (pSurface pop) then rowAt pop y else Nothing
        when (h /= pHover pop) $ modifyIORef' st $ \s' -> s'
          { sPopup = Just pop { pHover = h }
          , sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (pSurface pop) (sSurfs s') }
    click se btn = do
      s <- readIORef st
      forM_ (sFocus se >>= \surf -> (,) surf <$> M.lookup surf (sBars s)) $ \(surf, b) ->
        forM_ [ (x0, x1, f) | (x0, x1, f) <- bHits b, sX se >= x0, sX se < x1 ] $ \(x0, x1, f) ->
          void $ forkIO $ void $ E.try @E.SomeException $ f Click
            { clickButton = btn
            , clickOutput = bOutputName b
            , clickArea = (x0, x1)
            , clickMenu = ePush env . MsgMenu surf (x0, x1)
            }

------------------------------------------------------------------------------

utf8 :: B.ByteString -> String
utf8 = T.unpack . TE.decodeUtf8Lenient

encodeUtf8 :: String -> B.ByteString
encodeUtf8 = TE.encodeUtf8 . T.pack
