{-# LANGUAGE OverloadedStrings #-}
-- | The Wayland side: one layer-shell surface per output, drawn with
-- cairo/pango into shared memory buffers, plus popup menus, the popovers
-- that show hover markup and the side panel (a thin strip at an edge of
-- every output, and the panel it slides out).
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
import Data.List (find, findIndex, sortBy, zip4)
import Data.Ord (Down (..), comparing)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Clock (getMonotonicTime)
import System.Directory (listDirectory)
import System.Environment (setEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (Fd (..))

import Waymobar.Audio (Volume (..), onVolumeChange)
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
  | MsgGrid ObjectId (Double, Double) IconGrid       -- ^ bar surface, anchor, the grid
  | MsgGridTimeout Int                              -- ^ close the grid if the pointer is still away
  | MsgTick                                         -- ^ nothing to do but look at the clock (animations)
  | MsgPanelOpen Int                                -- ^ open the side panel if the pointer still rests at the edge
  | MsgPanelHide Int                                -- ^ close it if the pointer is still away
  | MsgCorner Int ObjectId                          -- ^ run a hot corner's action if the pointer still rests there
  | MsgOsd String (Maybe Double)                    -- ^ show the on-screen display
  | MsgOsdHide Int                                  -- ^ let it fade, if nothing was shown since

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
  , oStrip  :: Maybe ObjectId   -- ^ the wl_surface of the side panel's strip
  , oCorners :: Bool            -- ^ its hot corners exist
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
  , bTips       :: [(Double, Double, String)]  -- ^ blocks with hover markup, shown in a popover
  , bFrame      :: Maybe ObjectId              -- ^ pending frame callback while a ticker scrolls
  }

-- | The popover showing the hover markup of a block. It takes no input:
-- the pointer stays on the bar.
data Tip = Tip
  { tSurface :: ObjectId
  , tXdgSurf :: ObjectId
  , tPopup   :: ObjectId
  , tBar     :: ObjectId                -- ^ parent bar surface
  , tArea    :: (Double, Double)        -- ^ the block it belongs to
  , tMarkup  :: String
  , tFrom    :: Double                  -- ^ opacity the running fade started at
  , tTo      :: Double                  -- ^ opacity it ends at: 1 (showing) or 0 (going away)
  , tStart   :: Double                  -- ^ when it started (monotonic seconds)
  , tFrame   :: Maybe ObjectId          -- ^ pending frame callback
  }

-- | The invisible strip along an edge of an output that opens the side
-- panel when the pointer is pushed against it.
data Strip = Strip
  { stSurface :: ObjectId
  , stLayer   :: ObjectId
  , stOutput  :: ObjectId
  }

-- | A 1x1 invisible surface in a corner of an output (a hot corner).
data HotCorner = HotCorner
  { hcSurface :: ObjectId
  , hcLayer   :: ObjectId
  , hcOutput  :: ObjectId
  , hcCorner  :: Corner
  }

-- | The on-screen display, while it shows (or fades in or away).
data OsdPop = OsdPop
  { oSurface :: ObjectId
  , oLayer   :: ObjectId
  , oOut     :: ObjectId
  , oMarkup  :: String
  , oValue   :: Maybe Double
  , oFrom    :: Double             -- ^ opacity the running fade started at
  , oTo      :: Double             -- ^ 1 (showing) or 0 (going away)
  , oStart   :: Double
  , oFrame   :: Maybe ObjectId
  , oGen     :: Int                -- ^ bumped on every change, for the timer
  }

-- | The side panel, while it is out (or sliding in or away).
data Panel = Panel
  { plSurface    :: ObjectId
  , plLayer      :: ObjectId
  , plOutput     :: ObjectId
  , plOutputName :: String
  , plBox        :: (Double, Double)   -- ^ left and right edge of the panel in its surface
  , plHits       :: [(Double, Double, Click -> IO ())]  -- ^ rows that take clicks (y ranges)
  , plRows       :: [(Double, Double)] -- ^ rows whose look changes under the pointer
  , plTips       :: [(Double, Double, String)]  -- ^ rows with hover markup
  , plHoverY     :: Maybe Double
  , plFrom       :: Double             -- ^ like a popover's fade: 1 = out, 0 = hidden
  , plTo         :: Double
  , plStart      :: Double
  , plFrame      :: Maybe ObjectId
  }

-- | An open grid of icons.
data GridPop = GridPop
  { gpSurface :: ObjectId
  , gpXdgSurf :: ObjectId
  , gpPopup   :: ObjectId
  , gpBar     :: ObjectId               -- ^ parent bar surface
  , gpArea    :: (Double, Double)       -- ^ the block it opened from
  , gpGrid    :: IconGrid
  , gpCell    :: (Double, Double)       -- ^ size of a cell
  , gpScroll  :: Int                    -- ^ first row shown
  , gpHover   :: Maybe Int              -- ^ entry under the pointer
  , gpAway    :: Int                    -- ^ bumped whenever the pointer leaves our surfaces
  , gpStart   :: Double                 -- ^ when its fade-in started
  , gpFrame   :: Maybe ObjectId
  , gpAxis    :: Double                 -- ^ accumulated scroll
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
  , pStart   :: Maybe Double            -- ^ when its fade-in started ('Nothing' = shown at once)
  , pFrame   :: Maybe ObjectId          -- ^ pending frame callback
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
  , sTip       :: Maybe Tip
  , sGrid      :: Maybe GridPop
  , sStrips    :: M.Map ObjectId Strip    -- ^ by wl_surface
  , sCorners   :: M.Map ObjectId HotCorner  -- ^ by wl_surface
  , sCornerGen :: Int                     -- ^ bumped when the pointer enters or leaves a surface
  , sOsd       :: Maybe OsdPop
  , sPanel     :: Maybe Panel
  , sPanelGen  :: Int                     -- ^ bumped when the pointer enters or leaves a surface
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
  , ePanel   :: [Output -> IO [Block]]  -- ^ the side panel's widgets
  , eState   :: IORef S
  , eSync    :: ObjectId
  , ePush    :: Msg -> IO ()
  , eTickers :: IORef (M.Map String (Double, Double))  -- ^ ticker markup -> (when it appeared, when last drawn)
  , eRiver   :: IO (Maybe R.RState)     -- ^ rivermonad's state, for the focused output
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
        , showOsd = \m v -> when (isJust (osd cfg)) (push (MsgOsd m v))
        }
  forM_ (osd cfg) $ \oc -> startOsdSources oc client push
  ws <- (,,) <$> mapM (`startWidget` benv) (left cfg)
             <*> mapM (`startWidget` benv) (center cfg)
             <*> mapM (`startWidget` benv) (right cfg)
  pws <- mapM (`startWidget` benv) (maybe [] panelWidgets (sidePanel cfg))
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
    , sOutputs = M.empty, sSurfs = M.empty, sBars = M.empty, sPopup = Nothing, sTip = Nothing, sGrid = Nothing
    , sStrips = M.empty, sCorners = M.empty, sCornerGen = 0, sOsd = Nothing, sPanel = Nothing, sPanelGen = 0
    , sSeats = M.empty, sLastPress = Nothing, sReady = False }
  tickers <- newIORef M.empty
  let env = Env conn reg cfg h ws pws st sync push tickers (R.currentState client)
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
    modifyIORef' (eState env) $ \s ->
      let ours k = M.member k (sBars s) || Just k == (plSurface <$> sPanel s)
      in s { sSurfs = M.mapWithKey (\k sf -> if ours k then sf { sfDirty = True } else sf) (sSurfs s) }
    pure False
  MsgMenu bar area entries -> closeGrid env >> openMenu env bar area entries [] True >> pure False
  MsgGrid bar area g -> openGrid env bar area g >> pure False
  MsgGridTimeout gen -> do
    s <- readIORef (eState env)
    let onUs = any (isJust . sFocus) (M.elems (sSeats s))
    forM_ (sGrid s) $ \gp -> when (gpAway gp == gen && not onUs) (closeGrid env)
    pure False
  MsgMenuTimeout gen -> do
    -- river does not dismiss popups on outside clicks: close the menu once
    -- the pointer has been away from it (and from the bar) for a while
    s <- readIORef (eState env)
    let onUs = any (isJust . sFocus) (M.elems (sSeats s))
    forM_ (sPopup s) $ \p -> when (pAway p == gen && not onUs) (closeMenu env)
    pure False
  MsgTick -> pure False
  MsgPanelOpen gen -> panelOpen env gen >> pure False
  MsgPanelHide gen -> panelHide env gen >> pure False
  MsgCorner gen surf -> cornerFire env gen surf >> pure False
  MsgOsd m v -> osdShow env m v >> pure False
  MsgOsdHide gen -> osdHide env gen >> pure False
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
      destroyPanelOn env o
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
  EvWlCallback cb (WlCallbackDone _) -> do
    -- the compositor is ready for the next frame of a fade
    mp <- sPopup <$> readIORef st
    forM_ mp $ \p -> when (pFrame p == Just cb) $ do
      modifyIORef' st $ \s -> s { sPopup = Just p { pFrame = Nothing } }
      updSurf (pSurface p) (\sf -> sf { sfDirty = True })
    mt <- sTip <$> readIORef st
    forM_ mt $ \t -> when (tFrame t == Just cb) $ do
      modifyIORef' st $ \s -> s { sTip = Just t { tFrame = Nothing } }
      updSurf (tSurface t) (\sf -> sf { sfDirty = True })
    mgp <- sGrid <$> readIORef st
    forM_ mgp $ \gp -> when (gpFrame gp == Just cb) $ do
      modifyIORef' st $ \s -> s { sGrid = Just gp { gpFrame = Nothing } }
      updSurf (gpSurface gp) (\sf -> sf { sfDirty = True })
    bars <- sBars <$> readIORef st
    forM_ [ b | b <- M.elems bars, bFrame b == Just cb ] $ \b -> do
      modifyIORef' st $ \s -> s { sBars = M.adjust (\b' -> b' { bFrame = Nothing }) (bSurface b) (sBars s) }
      updSurf (bSurface b) (\sf -> sf { sfDirty = True })
    mo <- sOsd <$> readIORef st
    forM_ mo $ \o -> when (oFrame o == Just cb) $ do
      modifyIORef' st $ \s -> s { sOsd = Just o { oFrame = Nothing } }
      updSurf (oSurface o) (\sf -> sf { sfDirty = True })
    mpl <- sPanel <$> readIORef st
    forM_ mpl $ \pl -> when (plFrame pl == Just cb) $ do
      modifyIORef' st $ \s -> s { sPanel = Just pl { plFrame = Nothing } }
      updSurf (plSurface pl) (\sf -> sf { sfDirty = True })
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
    s <- readIORef st
    let configure surf lse' = case lse' of
          ZwlrLayerSurfaceV1Configure serial w h -> do
            zwlrLayerSurfaceV1AckConfigure c ls serial
            updSurf surf $ \sf -> sf
              { sfW = if w == 0 then max 1 (sfW sf) else fromIntegral w
              , sfH = if h == 0 then max 1 (sfH sf) else fromIntegral h
              , sfConfigured = True
              , sfDirty = True }
          ZwlrLayerSurfaceV1Closed -> pure ()
    forM_ (find ((== ls) . stLayer) (M.elems (sStrips s))) $ \t -> case lse of
      ZwlrLayerSurfaceV1Closed -> destroyStrip env t
      _ -> configure (stSurface t) lse
    forM_ (find ((== ls) . hcLayer) (M.elems (sCorners s))) $ \hc -> case lse of
      ZwlrLayerSurfaceV1Closed -> destroyCorner env hc
      _ -> configure (hcSurface hc) lse
    forM_ (sPanel s) $ \pl -> when (plLayer pl == ls) $ case lse of
      ZwlrLayerSurfaceV1Closed -> closePanel env
      _ -> configure (plSurface pl) lse
    forM_ (sOsd s) $ \o -> when (oLayer o == ls) $ case lse of
      ZwlrLayerSurfaceV1Closed -> closeOsd env
      _ -> configure (oSurface o) lse
  EvXdgWmBase wm (XdgWmBasePing serial) -> xdgWmBasePong c wm serial
  EvXdgSurface xs (XdgSurfaceConfigure serial) -> do
    mp <- sPopup <$> readIORef st
    forM_ mp $ \p -> when (pXdgSurf p == xs) $ do
      xdgSurfaceAckConfigure c xs serial
      updSurf (pSurface p) (\sf -> sf { sfConfigured = True, sfDirty = True })
    mt <- sTip <$> readIORef st
    forM_ mt $ \t -> when (tXdgSurf t == xs) $ do
      xdgSurfaceAckConfigure c xs serial
      updSurf (tSurface t) (\sf -> sf { sfConfigured = True, sfDirty = True })
    mgp <- sGrid <$> readIORef st
    forM_ mgp $ \gp -> when (gpXdgSurf gp == xs) $ do
      xdgSurfaceAckConfigure c xs serial
      updSurf (gpSurface gp) (\sf -> sf { sfConfigured = True, sfDirty = True })
  EvXdgPopup xp XdgPopupPopupDone -> do
    mp <- sPopup <$> readIORef st
    forM_ mp $ \p -> when (pPopup p == xp) (closeMenu env)
    mt <- sTip <$> readIORef st
    forM_ mt $ \t -> when (tPopup t == xp) (closeTip env)
    mgp <- sGrid <$> readIORef st
    forM_ mgp $ \gp -> when (gpPopup gp == xp) (closeGrid env)
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
      modifyIORef' st $ \s -> s { sOutputs = M.insert o (Out Nothing 1 False name Nothing Nothing False) (sOutputs s) }
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
  forM_ (M.elems (sBars s)) $ \b -> forM_ (M.lookup (bSurface b) (sSurfs s)) $ \sf ->
    when (sfDirty sf && sfConfigured sf) $ do
      let surf = sfSurface sf
      res <- present env sf $ \cv -> do
        (hs, hr, ts, moving) <- paintBar env cv (bOutputName b) (bHoverX b) (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
        -- a ticker scrolls: draw again as soon as the compositor shows this frame
        f <- if moving && isNothing (bFrame b)
          then Just <$> wlSurfaceFrame (eConn env) surf
          else pure Nothing
        pure (hs, hr, ts, f)
      forM_ res $ \(hs, hr, ts, f) -> modifyIORef' (eState env) $ \s' -> s'
        { sBars = M.adjust (\b' -> b' { bHits = hs, bHoverRects = hr, bTips = ts, bFrame = maybe (bFrame b') Just f }) surf (sBars s') }
  -- the strips of the side panel and the hot corners: nothing to see
  forM_ (map stSurface (M.elems (sStrips s)) ++ map hcSurface (M.elems (sCorners s))) $ \surf ->
    forM_ (M.lookup surf (sSurfs s)) $ \sf ->
      when (sfDirty sf && sfConfigured sf) $ void $ present env sf $ \cv -> clear cv (Color 0 0 0 0)
  -- the bars know what is under the pointer now
  syncTip env
  syncPanel env
  -- closing frees a surface's buffers: before reading the state the
  -- surfaces are drawn from, or a closed one would be drawn into freed memory
  syncOsd env
  s' <- readIORef (eState env)
  let dirty surf = [ sf | Just sf <- [M.lookup surf (sSurfs s')], sfDirty sf, sfConfigured sf ]
  forM_ (sPopup s') $ \p -> forM_ (dirty (pSurface p)) $ \sf -> do
    now <- getMonotonicTime
    let a = menuAlpha (eCfg env) p now
    cb <- present env sf $ \cv -> do
      paintMenu env cv p a (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
      if a < 1 && isNothing (pFrame p)
        then Just <$> wlSurfaceFrame (eConn env) (sfSurface sf)
        else pure Nothing
    forM_ (join cb) $ \f -> modifyIORef' (eState env) $ \x ->
      x { sPopup = (\p' -> p' { pFrame = Just f }) <$> sPopup x }
  forM_ (sTip s') $ \t -> forM_ (dirty (tSurface t)) $ \sf -> do
    now <- getMonotonicTime
    let a = tipAlpha (eCfg env) t now
    -- while it fades, draw again as soon as the compositor shows this frame
    cb <- present env sf $ \cv -> do
      paintTip env cv t a (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
      if a /= tTo t && isNothing (tFrame t)
        then Just <$> wlSurfaceFrame (eConn env) (sfSurface sf)
        else pure Nothing
    forM_ (join cb) $ \f -> modifyIORef' (eState env) $ \x ->
      x { sTip = (\t' -> t' { tFrame = Just f }) <$> sTip x }
  forM_ (sGrid s') $ \gp -> forM_ (dirty (gpSurface gp)) $ \sf -> do
    now <- getMonotonicTime
    let a = gridAlpha (eCfg env) gp now
    cb <- present env sf $ \cv -> do
      paintGrid env cv gp a (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
      if a < 1 && isNothing (gpFrame gp)
        then Just <$> wlSurfaceFrame (eConn env) (sfSurface sf)
        else pure Nothing
    forM_ (join cb) $ \f -> modifyIORef' (eState env) $ \x ->
      x { sGrid = (\g' -> g' { gpFrame = Just f }) <$> sGrid x }
  forM_ (sOsd s') $ \o -> forM_ (dirty (oSurface o)) $ \sf -> do
    now <- getMonotonicTime
    let a = osdAlpha o now
    cb <- present env sf $ \cv -> do
      paintOsd env cv o a (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
      if a /= oTo o && isNothing (oFrame o)
        then Just <$> wlSurfaceFrame (eConn env) (sfSurface sf)
        else pure Nothing
    forM_ (join cb) $ \f -> modifyIORef' (eState env) $ \x ->
      x { sOsd = (\o' -> o' { oFrame = Just f }) <$> sOsd x }
  forM_ (sPanel s') $ \pl -> forM_ (dirty (plSurface pl)) $ \sf -> do
    now <- getMonotonicTime
    let a = panelAlpha (eCfg env) pl now
    res <- present env sf $ \cv -> do
      r <- paintPanel env cv pl a (fromIntegral (sfW sf)) (fromIntegral (sfH sf))
      f <- if a /= plTo pl && isNothing (plFrame pl)
        then Just <$> wlSurfaceFrame (eConn env) (sfSurface sf)
        else pure Nothing
      pure (r, f)
    forM_ res $ \((hs, rows, tips), f) -> modifyIORef' (eState env) $ \x ->
      x { sPanel = (\p' -> p' { plHits = hs, plRows = rows, plTips = tips, plFrame = maybe (plFrame p') Just f }) <$> sPanel x }

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
    forM_ (M.toList (sOutputs s)) $ \(o, out) -> when (oDone out && wanted (oName out)) $ do
      when (isNothing (oBar out)) $ createBar env o out
      forM_ (sidePanel (eCfg env)) $ \sp -> when (isNothing (oStrip out)) $ createStrip env sp o
      when (not (oCorners out) && not (null (hotCorners (eCfg env)))) $ createCorners env o
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
      , sBars = M.insert surf (Bar surf ls o (fromMaybe "" (oName out)) [] Nothing [] [] Nothing) (sBars s)
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
  mt <- sTip <$> readIORef (eState env)
  forM_ mt $ \t -> when (tBar t == bSurface b) (closeTip env)
  mgp <- sGrid <$> readIORef (eState env)
  forM_ mgp $ \gp -> when (gpBar gp == bSurface b) (closeGrid env)
  zwlrLayerSurfaceV1Destroy (eConn env) (bLayer b)
  destroySurf env (bSurface b)
  modifyIORef' (eState env) $ \s -> s
    { sBars = M.delete (bSurface b) (sBars s)
    , sOutputs = M.adjust (\x -> x { oBar = Nothing }) (bOutput b) (sOutputs s) }

------------------------------------------------------------------------------
-- Drawing bars

-- | Draw a bar; returns its click areas, the areas whose look changes
-- under the pointer, and the blocks with hover markup (for the popover).
paintBar :: Env -> Canvas -> Output -> Maybe Double -> Double -> Double
         -> IO ([(Double, Double, Click -> IO ())], [(Double, Double)], [(Double, Double, String)], Bool)
paintBar env cv out hoverX w h = do
  let cfg = eCfg env
      (ls, cs, rs) = eWidgets env
      hasIcon b = isJust (blockIcon b) || isJust (blockIconSource b)
      visible b = not (null (blockMarkup b)) || hasIcon b || isJust (blockTicker b)
      -- widgets with nothing to show take no room (nor spacing)
      blocksOf = fmap (filter (not . null)) . mapM (\f -> filter visible <$> safe (f out))
  -- the bar itself: transparent outside its (possibly rounded) box
  clear cv (Color 0 0 0 0)
  setColor cv (color (bgColor cfg))
  fillRounded cv 0 0 w h (barRadius cfg)
  forM_ (barBorder cfg) $ \(bw, bc) -> setColor cv (color bc) >> strokeRounded cv 0 0 w h (barRadius cfg) bw
  devScale <- canvasScale cv
  now <- getMonotonicTime
  moving <- newIORef False
  -- when every ticker first appeared (its scroll starts from there)
  let tickerStart m = atomicModifyIORef' (eTickers env) $ \mp ->
        let t0 = maybe now fst (M.lookup m mp)
        in (M.filter (\(_, seen) -> now - seen < 1) (M.insert m (t0, now) mp), t0)
      -- Icons with a source are rasterized at the real (device) size, so
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
  withFont (font cfg) $ \fnt -> do
    let iconSize b = fromIntegral (if blockIconSize b > 0 then blockIconSize b else max 8 (round h - 8))
        iconPart b
          | hasIcon b = iconSize b + (if null (blockMarkup b) then 0 else 4)
          | otherwise = 0
        measure bss k = withLayouts cv fnt (map blockMarkup (concat bss)) $ \lays ->
         withLayouts cv fnt (map (maybe "" tickerMarkup . blockTicker) (concat bss)) $ \tlays -> do
          ws <- forM (zip3 (concat bss) lays tlays) $ \(b, l, tl) -> do
            (tw, th) <- layoutSize l
            tsz <- layoutSize tl
            let st = blockStyle b
                (_, mh) = boxMargin st
                textW = if null (blockMarkup b) then 0 else fromIntegral tw
                tick = (\t -> (t, tl, tsz)) <$> blockTicker b
                inner = iconPart b + textW + maybe 0 tickerWindow tick + 2 * (fromIntegral (blockPadding b) + boxPadding st)
            pure (max (fromIntegral (blockMinWidth b)) inner + 2 * mh, (th, tick))
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
        drawGroups x0 gs = fmap cat3 $ forM (zip (scanl (\x g -> x + groupWidth g + sp) x0 gs) gs) $ \(gx, g) -> do
          forM_ (groupStyleOf g) $ \gs -> do
            let gst = if hovered gx (groupWidth g) then maybe gs (`mergeStyle` gs) (styleHover gs) else gs
            drawBox cv gst gx (groupWidth g) h
          fmap cat3 $ forM (zip (scanl (+) (gx + groupInset g) [ bw | (_, _, (bw, _)) <- g ]) g) $ \(x, (b, l, (bw, (th, tick)))) -> do
            let base = blockStyle b
                st = if hovered x bw then maybe base (`mergeStyle` base) (styleHover base) else base
            ic <- blockBitmap b st (iconSize b)
            tx <- drawBlock cfg cv b { blockIcon = ic, blockStyle = st } l x bw h th (iconSize b)
            forM_ tick $ \tk@(t, tl, (ttw, tth)) -> do
              let win = tickerWindow tk
                  tw' = fromIntegral ttw
                  -- what fits stays still; otherwise it scrolls, stopping
                  -- for 'tickerPause' every time it is back at its start
                  scrolls = tw' > win
                  period = tw' + fromIntegral (max 0 (tickerGap t))
                  speed = max 1 (tickerSpeed t)
                  pause = max 0 (tickerPause t)
                  cyc = pause + period / speed
                  ty = fromIntegral (round ((h - fromIntegral tth) / 2) :: Int)
                  -- whole device pixels, so the glyphs don't shimmer
                  snap v = fromIntegral (round (v * devScale) :: Int) / devScale
              off <- if not scrolls then pure 0 else do
                t0 <- tickerStart (tickerMarkup t)
                writeIORef moving True
                let tc = (now - t0) - cyc * fromIntegral (floor ((now - t0) / cyc) :: Int)
                pure (snap (max 0 (tc - pause) * speed))
              let fade = 14
              setColor cv (color (fromMaybe (fgColor cfg) (styleFg st)))
              withEdgeFade cv tx 0 win h (if scrolls then min fade off else 0) (if scrolls then fade else 0) $ do
                showLayout cv tl (tx - off) ty
                when scrolls $ showLayout cv tl (tx - off + period) ty
            pure ( [ (x, x + bw, f) | Just f <- [blockOnClick b] ]
                 , [ (x, x + bw) | isJust (styleHover base) ]
                 , [ (x, x + bw, m) | Just m <- [blockHoverMarkup b], not (null m) ] )
        cat3 xs = (concat [ a | (a, _, _) <- xs ], concat [ b | (_, b, _) <- xs ], concat [ c | (_, _, c) <- xs ])
        -- hover areas also cover the boxes of whole widgets
        groupRects x0 gs = [ (gx, gx + groupWidth g)
                           | (gx, g) <- zip (scanl (\x g -> x + groupWidth g + sp) x0 gs) gs
                           , Just gs' <- [groupStyleOf g], isJust (styleHover gs') ]
    measure lb0 $ \lgs -> measure cb0 $ \cgs0 -> measure rb0 $ \rgs -> do
      -- The center gets the room the left and right sides leave: when it
      -- does not fit, its longest texts are cut with "…" (and it goes away
      -- when there is next to no room at all).
      let room = w - 2 * pad - totalWidth lgs - totalWidth rgs - 2 * sp
      cgs <- if totalWidth cgs0 <= room then pure cgs0
             else if room < 40 then pure []
             else fitCenter (totalWidth cgs0 - room) cgs0
      let lw = totalWidth lgs
          rw = totalWidth rgs
          cw = totalWidth cgs
          rx = w - pad - rw
          cx = max (pad + lw + sp) (min (rx - sp - cw) ((w - cw) / 2))
      (h1, r1, t1) <- drawGroups pad lgs
      (h2, r2, t2) <- drawGroups cx cgs
      (h3, r3, t3) <- drawGroups rx rgs
      mv <- readIORef moving
      pure ( h1 ++ h2 ++ h3
           , r1 ++ r2 ++ r3 ++ groupRects pad lgs ++ groupRects cx cgs ++ groupRects rx rgs
           , t1 ++ t2 ++ t3
           , mv )
  where
    safe act = act `E.catch` \(e :: E.SomeException) -> do
      hPutStrLn stderr ("waymobar: widget failed: " ++ show e)
      pure []
    regroup [] _ = []
    regroup (g : gs) xs = let (a, b) = splitAt (length g) xs in a : regroup gs b
    -- Shrink the texts of the center by @over@ pixels in all, the longest
    -- first (each one down to 'minText' at most).
    fitCenter over gs = do
      let flat = concat gs
      tws <- forM flat $ \(b, l, _) -> if null (blockMarkup b) then pure 0 else fromIntegral . fst <$> layoutSize l
      let order = map snd (sortBy (comparing (Down . fst)) (zip tws [0 :: Int ..]))
          cuts = snd (foldl (\(left, acc) i ->
                    let c = max 0 (min left ((tws !! i) - minText)) in (left - c, M.insert i c acc))
                    (over, M.empty) order)
      flat' <- forM (zip3 [0 ..] flat tws) $ \(i, (b, l, (bw, x)), tw) -> case M.lookup i cuts of
        Just c | c > 0 -> do
          ellipsizeLayout l (tw - c)
          (nw, _) <- layoutSize l
          pure (b, l, (bw - (tw - fromIntegral nw), x))
        _ -> pure (b, l, (bw, x))
      pure (regroup gs flat')
    minText = 60
    -- the window of a ticker: its width, or less when the text is shorter
    tickerWindow (t, _, (ttw, _)) = min (fromIntegral (max 0 (tickerWidth t))) (fromIntegral ttw)

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

-- | Draw a block; returns where its markup ends (a ticker goes there).
drawBlock :: Config -> Canvas -> Block -> TextLayout -> Double -> Double -> Double -> Int -> Double -> IO Double
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
  if null (blockMarkup b) then pure tx else do
    setColor cv (color (fromMaybe (fgColor cfg) (styleFg st)))
    showLayout cv l tx (fromIntegral (round ((h - fromIntegral th) / 2) :: Int))
    (tw, _) <- layoutSize l
    pure (tx + fromIntegral tw)

------------------------------------------------------------------------------
-- Menus

menuPad, menuBorderW, menuInset :: Double
menuPad = 5
menuBorderW = 1
menuInset = 5   -- between the panel's edge and the highlight of a row

-- | The opacity of a menu now (it fades in like a popover).
menuAlpha :: Config -> Popup -> Double -> Double
menuAlpha cfg p now = case pStart p of
  Just t0 | d > 0 -> 1 - (1 - max 0 (min 1 ((now - t0) / d))) ^ (3 :: Int)
  _ -> 1
  where d = fromIntegral (max 0 (popoverFade cfg)) / 1000 :: Double

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

-- | Size of a menu's surface (shadow included) and the y range of every row.
measureMenu :: Config -> [Row] -> IO (Int, Int, [(Double, Double)])
measureMenu cfg rows =
  allocaBytes 4 $ \p -> withCanvas p 1 1 1 $ \cv -> withFont (font cfg) $ \fnt ->
    withLayouts cv fnt (map (escapeMarkup . rowLabel) rows) $ \lays -> do
      sizes <- mapM layoutSize lays
      let lineH = fromIntegral (maximum (16 : map snd sizes)) + 8
          heights = [ if isSeparator r then 9 else lineH | r <- rows ]
          mg = fromIntegral tipMargin
          ys = scanl (+) (mg + menuBorderW + menuPad) heights
          textW = fromIntegral (maximum (0 : map fst sizes))
          w = 2 * mg + 2 * menuBorderW + leftGutter rows + textW + 36
          h = last ys + menuPad + menuBorderW + mg
      pure (ceiling w, ceiling h, zip ys (drop 1 ys))

leftGutter :: [Row] -> Double
leftGutter rows = 14 + (if any hasMark rows then 18 else 0) + (if any hasIcon rows then 22 else 0)
  where
    hasMark = \case
      RowEntry MenuItem { itemToggle = Just _ } -> True
      _ -> False
    hasIcon = \case
      RowEntry MenuItem { itemIcon = Just _ } -> True
      _ -> False

-- | Open a menu under a block of a bar; it fades in if asked (not when
-- moving between a menu and its submenus).
openMenu :: Env -> ObjectId -> (Double, Double) -> [MenuEntry] -> [[MenuEntry]] -> Bool -> IO ()
openMenu env barSurf area entries stack animate = do
  closeMenu env
  closeTip env
  s <- readIORef st
  let g = sGlobals s
      cfg = eCfg env
      rows = rowsFor entries stack
      -- a bar, or the side panel
      parent = case (M.lookup barSurf (sBars s), sPanel s) of
        (Just bar, _) -> Just (bLayer bar, bOutput bar, Nothing)
        (Nothing, Just pl) | plSurface pl == barSurf -> Just (plLayer pl, plOutput pl, Just pl)
        _ -> Nothing
  case (gWmBase g, parent, sLastPress s) of
    (Just wm, Just (parentLayer, parentOutput, mpanel), Just (seat, serial)) | not (null rows) -> do
      (w, h, ys) <- measureMenu cfg rows
      msf <- newSurf env parentOutput
      forM_ msf $ \sf -> do
        let surf = sfSurface sf
            (x0, x1) = area
            barSf = M.lookup barSurf (sSurfs s)
            barH = maybe (eHeight env) sfH barSf
            mg = fromIntegral tipMargin
            off = round tipGap - mg
        -- only the panel takes input, not the room for its shadow
        forM_ (gCompositor g) $ \(comp, _) -> do
          region <- wlCompositorCreateRegion c comp
          wlRegionAdd c region mg mg (fromIntegral w - 2 * mg) (fromIntegral h - 2 * mg)
          wlSurfaceSetInputRegion c surf region
          wlRegionDestroy c region
        xs <- xdgWmBaseGetXdgSurface c wm surf
        pos <- xdgWmBaseCreatePositioner c wm
        xdgPositionerSetSize c pos (fromIntegral w) (fromIntegral h)
        case mpanel of
          Nothing -> do
            xdgPositionerSetAnchorRect c pos (round x0) 0 (max 1 (round (x1 - x0))) (fromIntegral barH)
            -- the panel lines up with the left edge of the block
            if position cfg == Top
              then do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorBottomLeft
                xdgPositionerSetGravity c pos xdgPositionerGravityBottomRight
                xdgPositionerSetOffset c pos (negate mg) off
              else do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorTopLeft
                xdgPositionerSetGravity c pos xdgPositionerGravityTopRight
                xdgPositionerSetOffset c pos (negate mg) (negate off)
            xdgPositionerSetConstraintAdjustment c pos
              (xdgPositionerConstraintAdjustmentSlideX .|. xdgPositionerConstraintAdjustmentSlideY .|. xdgPositionerConstraintAdjustmentFlipY)
          Just pl -> do
            -- beside the side panel, its top in line with the row (the area
            -- is the row's y range there)
            let (bx0, bx1) = plBox pl
            xdgPositionerSetAnchorRect c pos (round bx0) (round x0) (max 1 (round (bx1 - bx0))) (max 1 (round (x1 - x0)))
            if panelEdgeOf cfg == RightEdge
              then do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorTopLeft
                xdgPositionerSetGravity c pos xdgPositionerGravityBottomLeft
                xdgPositionerSetOffset c pos (mg - round tipGap) (negate (mg + round menuPad + round menuBorderW))
              else do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorTopRight
                xdgPositionerSetGravity c pos xdgPositionerGravityBottomRight
                xdgPositionerSetOffset c pos (round tipGap - mg) (negate (mg + round menuPad + round menuBorderW))
            xdgPositionerSetConstraintAdjustment c pos
              (xdgPositionerConstraintAdjustmentSlideY .|. xdgPositionerConstraintAdjustmentFlipX)
        popup <- xdgSurfaceGetPopup c xs nullObject pos
        xdgPositionerDestroy c pos
        zwlrLayerSurfaceV1GetPopup c parentLayer popup
        xdgPopupGrab c popup seat serial
        wlSurfaceCommit c surf
        now <- getMonotonicTime
        modifyIORef' st $ \s' -> s'
          { sSurfs = M.insert surf sf { sfW = w, sfH = h
                                      , sfScale120 = maybe 0 sfScale120 barSf
                                      , sfBufScale = maybe 0 sfBufScale barSf } (sSurfs s')
          , sPopup = Just (Popup surf xs popup barSurf area rows ys stack Nothing 0
                             (if animate then Just now else Nothing) Nothing) }
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

paintMenu :: Env -> Canvas -> Popup -> Double -> Double -> Double -> IO ()
paintMenu env cv p alpha w h = do
  let cfg = eCfg env
      fg = color (fgColor cfg)
      dim = let Color r g b a = fg in Color r g b (a * 0.4)
      mg = fromIntegral tipMargin
      (px, pw) = (mg, w - 2 * mg)
      gutter = px + leftGutter (pRows p)
      markX = px + 14
      iconX = px + (if any isMark (pRows p) then 32 else 14)
      dir = if position cfg == Top then -1 else 1
  clear cv (Color 0 0 0 0)
  withFade cv alpha (dir * tipSlide * (1 - alpha)) $ do
    drawPanel cfg cv px mg pw (h - 2 * mg)
    withFont (font cfg) $ \fnt ->
      withLayouts cv fnt (map (escapeMarkup . rowLabel) (pRows p)) $ \lays ->
        forM_ (zip3 [0 ..] (pRows p) (zip lays (pLayout p))) $ \(i, row, (l, (y0, y1))) ->
          if isSeparator row
            then do
              setColor cv (color (menuBorder cfg))
              fillRect cv (px + 12) (fromIntegral (round ((y0 + y1) / 2) :: Int)) (pw - 24) 1
            else do
              let enabled = rowEnabled row
              when (pHover p == Just i && enabled) $ do
                setColor cv (color (menuHighlight cfg))
                fillRounded cv (px + menuInset) y0 (pw - 2 * menuInset) (y1 - y0) (max 0 (popoverRadius cfg - menuInset))
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
                RowEntry (Submenu _ _) -> withMarkup cv fnt "›" $ \m -> showLayout cv m (px + pw - 22) ty
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
      openMenu env (pBar p) (pArea p) es (currentEntries : pStack p) False
    RowBack -> case pStack p of
      (parent : rest) -> openMenu env (pBar p) (pArea p) parent rest False
      [] -> pure ()
    RowEntry MenuSeparator -> pure ()
  _ -> pure ()
  where
    currentEntries = [ e | RowEntry e <- pRows p ]

------------------------------------------------------------------------------
-- Popovers (hover markup)

-- | Transparent room around the panel of a popover, for its shadow.
tipMargin :: Int
tipMargin = 18

tipPadX, tipPadY, tipGap, tipSlide :: Double
tipPadX = 12
tipPadY = 8
tipGap = 6     -- between the bar and the panel
tipSlide = 6   -- how far the panel travels while it fades in

-- | The size of the popover's surface (shadow included) for some markup.
measureTip :: Config -> String -> IO (Int, Int)
measureTip cfg m =
  allocaBytes 4 $ \p -> withCanvas p 1 1 1 $ \cv -> withFont (font cfg) $ \fnt ->
    withMarkup cv fnt m $ \l -> do
      (tw, th) <- layoutSize l
      pure ( tw + 2 * (ceiling tipPadX + tipMargin), th + 2 * (ceiling tipPadY + tipMargin) )

-- | The opacity of a popover now: an ease-out from 'tFrom' to 'tTo'.
tipAlpha :: Config -> Tip -> Double -> Double
tipAlpha cfg t now = tFrom t + (tTo t - tFrom t) * (1 - (1 - p) ^ (3 :: Int))
  where
    d = tipDuration cfg t
    p = if d <= 0 then 1 else max 0 (min 1 ((now - tStart t) / d))

-- | Seconds a fade takes; going away is quicker than showing up.
tipDuration :: Config -> Tip -> Double
tipDuration cfg t = fromIntegral (max 0 (popoverFade cfg)) / 1000 * (if tTo t == 0 then 0.7 else 1)

-- | Open, update or close the popover after what is under the pointer.
syncTip :: Env -> IO ()
syncTip env = do
  s <- readIORef st
  now <- getMonotonicTime
  let cfg = eCfg env
      -- no popovers while a menu is open
      want = if isJust (sPopup s) || isJust (sGrid s) then Nothing else listToMaybe $
        [ (bSurface b, (x0, x1), m)
        | se <- M.elems (sSeats s), Just f <- [sFocus se], Just b <- [M.lookup f (sBars s)]
        , (x0, x1, m) <- bTips b, sX se >= x0, sX se < x1 ] ++
        -- beside the side panel the area is a row's y range
        [ (plSurface pl, (y0, y1), m)
        | Just pl <- [sPanel s], plTo pl == 1
        , se <- M.elems (sSeats s), sFocus se == Just (plSurface pl)
        , let (bx0, bx1) = plBox pl, sX se >= bx0, sX se < bx1
        , (y0, y1, m) <- plTips pl, sY se >= y0, sY se < y1 ]
      set t = modifyIORef' st $ \x -> x
        { sTip = Just t, sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (tSurface t) (sSurfs x) }
      fadeTo t to = t { tFrom = tipAlpha cfg t now, tTo = to, tStart = now }
  case (sTip s, want) of
    (Nothing, Nothing) -> pure ()
    (Nothing, Just (b, a, m)) -> openTip env b a m 0
    (Just t, Nothing)
      | tTo t == 0 -> when (now - tStart t >= tipDuration cfg t) (closeTip env)
      | otherwise -> do
          let t' = fadeTo t 0
          set t'
          -- frame callbacks stop if the surface is hidden: make sure it goes
          void $ forkIO $ threadDelay (round (tipDuration cfg t' * 1000000) + 20000) >> ePush env MsgTick
    (Just t, Just (b, a, m))
      -- another block: the new popover starts as visible as this one was,
      -- so moving along the bar switches them at once (like a menu bar)
      | tBar t /= b || tArea t /= a -> closeTip env >> openTip env b a m (tipAlpha cfg t now)
      | tMarkup t /= m -> do
          size <- measureTip cfg m
          if Just size == ((\sf -> (sfW sf, sfH sf)) <$> M.lookup (tSurface t) (sSurfs s))
            then set (fadeTo t 1) { tMarkup = m }
            else closeTip env >> openTip env b a m (tipAlpha cfg t now)
      | tTo t == 0 -> set (fadeTo t 1)
      | otherwise -> pure ()
  where st = eState env

-- | A popover for a block of a bar, starting at the given opacity.
openTip :: Env -> ObjectId -> (Double, Double) -> String -> Double -> IO ()
openTip env barSurf area@(x0, x1) m from = do
  s <- readIORef st
  let g = sGlobals s
      cfg = eCfg env
      parent = case (M.lookup barSurf (sBars s), sPanel s) of
        (Just bar, _) -> Just (bLayer bar, bOutput bar, Nothing)
        (Nothing, Just pl) | plSurface pl == barSurf -> Just (plLayer pl, plOutput pl, Just pl)
        _ -> Nothing
  case (gWmBase g, gCompositor g, parent) of
    (Just wm, Just (comp, _), Just (parentLayer, parentOutput, mpanel)) -> do
      (w, h) <- measureTip cfg m
      msf <- newSurf env parentOutput
      forM_ msf $ \sf -> do
        let surf = sfSurface sf
            barSf = M.lookup barSurf (sSurfs s)
            barH = maybe (eHeight env) sfH barSf
            -- the shadow's room overlaps the bar: only the gap separates
            -- the panel from it
            off = round tipGap - tipMargin
        -- no input: the pointer goes through to whatever is below
        region <- wlCompositorCreateRegion c comp
        wlSurfaceSetInputRegion c surf region
        wlRegionDestroy c region
        xs <- xdgWmBaseGetXdgSurface c wm surf
        pos <- xdgWmBaseCreatePositioner c wm
        xdgPositionerSetSize c pos (fromIntegral w) (fromIntegral h)
        case mpanel of
          Nothing -> do
            xdgPositionerSetAnchorRect c pos (round x0) 0 (max 1 (round (x1 - x0))) (fromIntegral barH)
            -- centred on the block
            if position cfg == Top
              then do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorBottom
                xdgPositionerSetGravity c pos xdgPositionerGravityBottom
                xdgPositionerSetOffset c pos 0 (fromIntegral off)
              else do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorTop
                xdgPositionerSetGravity c pos xdgPositionerGravityTop
                xdgPositionerSetOffset c pos 0 (fromIntegral (negate off))
            xdgPositionerSetConstraintAdjustment c pos xdgPositionerConstraintAdjustmentSlideX
          Just pl -> do
            -- beside the panel, centred on the row
            let (bx0, bx1) = plBox pl
            xdgPositionerSetAnchorRect c pos (round bx0) (round x0) (max 1 (round (bx1 - bx0))) (max 1 (round (x1 - x0)))
            if panelEdgeOf cfg == RightEdge
              then do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorLeft
                xdgPositionerSetGravity c pos xdgPositionerGravityLeft
                xdgPositionerSetOffset c pos (fromIntegral (negate off)) 0
              else do
                xdgPositionerSetAnchor c pos xdgPositionerAnchorRight
                xdgPositionerSetGravity c pos xdgPositionerGravityRight
                xdgPositionerSetOffset c pos (fromIntegral off) 0
            xdgPositionerSetConstraintAdjustment c pos xdgPositionerConstraintAdjustmentSlideY
        popup <- xdgSurfaceGetPopup c xs nullObject pos
        xdgPositionerDestroy c pos
        zwlrLayerSurfaceV1GetPopup c parentLayer popup
        wlSurfaceCommit c surf
        now <- getMonotonicTime
        modifyIORef' st $ \s' -> s'
          { -- the bar's scale is the best guess until the compositor says
            sSurfs = M.insert surf sf { sfW = w, sfH = h
                                      , sfScale120 = maybe 0 sfScale120 barSf
                                      , sfBufScale = maybe 0 sfBufScale barSf } (sSurfs s')
          , sTip = Just (Tip surf xs popup barSurf area m from 1 now Nothing) }
    _ -> pure ()
  where
    c = eConn env
    st = eState env

closeTip :: Env -> IO ()
closeTip env = do
  mt <- sTip <$> readIORef (eState env)
  forM_ mt $ \t -> do
    xdgPopupDestroy (eConn env) (tPopup t)
    xdgSurfaceDestroy (eConn env) (tXdgSurf t)
    destroySurf env (tSurface t)
    modifyIORef' (eState env) $ \s -> s { sTip = Nothing }

-- | The popover: its markup on a panel, at the given opacity; it slides
-- away from the bar into its place as it appears.
paintTip :: Env -> Canvas -> Tip -> Double -> Double -> Double -> IO ()
paintTip env cv t alpha w h = do
  s <- readIORef (eState env)
  let cfg = eCfg env
      mg = fromIntegral tipMargin
      dir = if position cfg == Top then -1 else 1
      -- beside the side panel it comes out of the panel, sideways
      side = isNothing (M.lookup (tBar t) (sBars s))
      sideDir = if panelEdgeOf cfg == RightEdge then 1 else -1
      (dx, dy) = if side then (sideDir * tipSlide * (1 - alpha), 0) else (0, dir * tipSlide * (1 - alpha))
  clear cv (Color 0 0 0 0)
  withFadeXY cv alpha dx dy $ do
    drawPanel cfg cv mg mg (w - 2 * mg) (h - 2 * mg)
    setColor cv (color (fgColor cfg))
    withFont (font cfg) $ \fnt -> withMarkup cv fnt (tMarkup t) $ \l ->
      showLayout cv l (mg + tipPadX) (mg + tipPadY)

-- | The rounded panel of popovers and menus, with a soft shadow around it
-- (which needs 'tipMargin' of room at every side).
drawPanel :: Config -> Canvas -> Double -> Double -> Double -> Double -> IO ()
drawPanel cfg cv px py pw ph = do
  let r = popoverRadius cfg
      spread = 14 :: Int
  -- the shadow: thin layers, each a pixel smaller, falling a bit below
  forM_ [spread, spread - 1 .. 1] $ \k -> do
    let d = fromIntegral k
        fall = 1 - (d - 1) / fromIntegral spread
    setColor cv (Color 0 0 0 (0.05 * fall * fall))
    fillRounded cv (px - d) (py - d + 3) (pw + 2 * d) (ph + 2 * d) (r + d)
  -- the panel replaces the shadow under it, so a see-through background
  -- shows what is behind it, not a dark smudge
  setColor cv (color (fromMaybe (bgColor cfg) (popoverBg cfg)))
  fillRoundedReplace cv px py pw ph r
  setColor cv (color (menuBorder cfg))
  strokeRounded cv px py pw ph r 1

------------------------------------------------------------------------------
-- Icon grids (application launchers)

gridPad, gridCellPad, gridGap :: Double
gridPad = 10       -- between the panel's edge and the cells
gridCellPad = 8    -- inside a cell, around the icon and the label
gridGap = 6        -- between the icon and its label

-- | The size of a cell and of the panel's surface (shadow included).
gridGeometry :: Config -> IconGrid -> IO ((Double, Double), (Int, Int))
gridGeometry cfg g = do
  lineH <- allocaBytes 4 $ \p -> withCanvas p 1 1 1 $ \cv -> withFont (font cfg) $ \fnt ->
    withMarkup cv fnt "Ág" $ \l -> snd <$> layoutSize l
  let isz = fromIntegral (max 16 (gridIconSize g))
      cols = max 1 (gridColumns g)
      rows = gridVisibleRows g
      cw = max 112 (isz + 64)
      ch = isz + 2 * gridCellPad + gridGap + 2 * fromIntegral lineH
      mg = fromIntegral tipMargin
      w = 2 * mg + 2 * gridPad + fromIntegral cols * cw
      h = 2 * mg + 2 * gridPad + fromIntegral rows * ch
  pure ((cw, ch), (ceiling w, ceiling h))

gridTotalRows :: IconGrid -> Int
gridTotalRows g = (length (gridEntries g) + max 1 (gridColumns g) - 1) `div` max 1 (gridColumns g)

gridVisibleRows :: IconGrid -> Int
gridVisibleRows g = max 1 (min (max 1 (gridRows g)) (gridTotalRows g))

-- | The entry under a point of the grid's surface.
gridIndexAt :: GridPop -> Double -> Double -> Maybe Int
gridIndexAt gp x y = do
  let (cw, ch) = gpCell gp
      g = gpGrid gp
      mg = fromIntegral tipMargin
      cols = max 1 (gridColumns g)
      cx = (x - mg - gridPad) / cw
      cy = (y - mg - gridPad) / ch
  guard (cx >= 0 && cy >= 0 && floor cx < cols && floor cy < gridVisibleRows g)
  let i = (floor cy + gpScroll gp) * cols + floor cx
  guard (i < length (gridEntries g))
  pure i

openGrid :: Env -> ObjectId -> (Double, Double) -> IconGrid -> IO ()
openGrid env barSurf area g = do
  closeMenu env
  closeTip env
  closeGrid env
  s <- readIORef st
  let gl = sGlobals s
      cfg = eCfg env
  case (gWmBase gl, gCompositor gl, M.lookup barSurf (sBars s), sLastPress s) of
    (Just wm, Just (comp, _), Just bar, Just (seat, serial)) | not (null (gridEntries g)) -> do
      (cell, (w, h)) <- gridGeometry cfg g
      msf <- newSurf env (bOutput bar)
      forM_ msf $ \sf -> do
        let surf = sfSurface sf
            (x0, x1) = area
            barSf = M.lookup barSurf (sSurfs s)
            barH = maybe (eHeight env) sfH barSf
            mg = fromIntegral tipMargin
            off = round tipGap - mg
        region <- wlCompositorCreateRegion c comp
        wlRegionAdd c region mg mg (fromIntegral w - 2 * mg) (fromIntegral h - 2 * mg)
        wlSurfaceSetInputRegion c surf region
        wlRegionDestroy c region
        xs <- xdgWmBaseGetXdgSurface c wm surf
        pos <- xdgWmBaseCreatePositioner c wm
        xdgPositionerSetSize c pos (fromIntegral w) (fromIntegral h)
        xdgPositionerSetAnchorRect c pos (round x0) 0 (max 1 (round (x1 - x0))) (fromIntegral barH)
        -- like a menu: its left edge in line with the block's
        if position cfg == Top
          then do
            xdgPositionerSetAnchor c pos xdgPositionerAnchorBottomLeft
            xdgPositionerSetGravity c pos xdgPositionerGravityBottomRight
            xdgPositionerSetOffset c pos (negate mg) off
          else do
            xdgPositionerSetAnchor c pos xdgPositionerAnchorTopLeft
            xdgPositionerSetGravity c pos xdgPositionerGravityTopRight
            xdgPositionerSetOffset c pos (negate mg) (negate off)
        xdgPositionerSetConstraintAdjustment c pos
          (xdgPositionerConstraintAdjustmentSlideX .|. xdgPositionerConstraintAdjustmentSlideY .|. xdgPositionerConstraintAdjustmentFlipY)
        popup <- xdgSurfaceGetPopup c xs nullObject pos
        xdgPositionerDestroy c pos
        zwlrLayerSurfaceV1GetPopup c (bLayer bar) popup
        xdgPopupGrab c popup seat serial
        wlSurfaceCommit c surf
        now <- getMonotonicTime
        modifyIORef' st $ \s' -> s'
          { sSurfs = M.insert surf sf { sfW = w, sfH = h
                                      , sfScale120 = maybe 0 sfScale120 barSf
                                      , sfBufScale = maybe 0 sfBufScale barSf } (sSurfs s')
          , sGrid = Just GridPop
              { gpSurface = surf, gpXdgSurf = xs, gpPopup = popup, gpBar = barSurf, gpArea = area
              , gpGrid = g, gpCell = cell, gpScroll = 0, gpHover = Nothing, gpAway = 0
              , gpStart = now, gpFrame = Nothing, gpAxis = 0 } }
    _ -> pure ()
  where
    c = eConn env
    st = eState env

closeGrid :: Env -> IO ()
closeGrid env = do
  mg <- sGrid <$> readIORef (eState env)
  forM_ mg $ \gp -> do
    xdgPopupDestroy (eConn env) (gpPopup gp)
    xdgSurfaceDestroy (eConn env) (gpXdgSurf gp)
    destroySurf env (gpSurface gp)
    modifyIORef' (eState env) $ \s -> s { sGrid = Nothing }

-- | The opacity of a grid now (it fades in like a menu).
gridAlpha :: Config -> GridPop -> Double -> Double
gridAlpha cfg gp now
  | d <= 0 = 1
  | otherwise = 1 - (1 - max 0 (min 1 ((now - gpStart gp) / d))) ^ (3 :: Int)
  where d = fromIntegral (max 0 (popoverFade cfg)) / 1000 :: Double

paintGrid :: Env -> Canvas -> GridPop -> Double -> Double -> Double -> IO ()
paintGrid env cv gp alpha w h = do
  let cfg = eCfg env
      g = gpGrid gp
      mg = fromIntegral tipMargin
      (cw, ch) = gpCell gp
      cols = max 1 (gridColumns g)
      rows = gridVisibleRows g
      isz = fromIntegral (max 16 (gridIconSize g))
      dir = if position cfg == Top then -1 else 1
      shown = take (rows * cols) (drop (gpScroll gp * cols) (zip [0 ..] (gridEntries g)))
  devScale <- canvasScale cv
  clear cv (Color 0 0 0 0)
  withFade cv alpha (dir * tipSlide * (1 - alpha)) $ do
    drawPanel cfg cv mg mg (w - 2 * mg) (h - 2 * mg)
    withFont (font cfg) $ \fnt -> forM_ (zip [0 :: Int ..] shown) $ \(k, (i, e)) -> do
      let x = mg + gridPad + fromIntegral (k `mod` cols) * cw
          y = mg + gridPad + fromIntegral (k `div` cols) * ch
      when (gpHover gp == Just i) $ do
        setColor cv (color (menuHighlight cfg))
        fillRounded cv (x + 2) (y + 2) (cw - 4) (ch - 4) (max 0 (popoverRadius cfg - menuInset))
      icon <- case gridIcon e of
        Nothing -> pure Nothing
        Just src -> resolveIcon src (max 1 (round (isz * devScale))) Nothing False
      let ix = x + (cw - isz) / 2
          iy = y + gridCellPad
      case icon of
        Just ic -> drawIcon cv ic ix iy isz
        Nothing -> do
          -- no icon: a rounded square with the first letter
          setColor cv (color (menuBorder cfg))
          fillRounded cv ix iy isz isz (isz / 4)
          setColor cv (color (fgColor cfg))
          withMarkup cv fnt ("<span size='x-large'><b>" ++ escapeMarkup (take 1 (gridLabel e)) ++ "</b></span>") $ \l -> do
            (tw, th) <- layoutSize l
            showLayout cv l (ix + (isz - fromIntegral tw) / 2) (iy + (isz - fromIntegral th) / 2)
      setColor cv (color (fgColor cfg))
      withMarkupBox cv fnt (cw - 2 * gridCellPad) 2 (escapeMarkup (gridLabel e)) $ \l ->
        showLayout cv l (x + gridCellPad) (iy + isz + gridGap)
    -- where the shown rows are among all of them
    let total = gridTotalRows g
    when (total > rows) $ do
      let trackH = h - 2 * mg - 2 * gridPad
          barH = max 24 (trackH * fromIntegral rows / fromIntegral total)
          top = mg + gridPad + (trackH - barH) * fromIntegral (gpScroll gp) / fromIntegral (total - rows)
          Color r gr b _ = color (fgColor cfg)
      setColor cv (Color r gr b 0.3)
      fillRounded cv (w - mg - gridPad / 2 - 2) top 3 barH 1.5

-- | Scroll a grid by some rows (negative: up).
scrollGrid :: Env -> Int -> IO ()
scrollGrid env n = do
  s <- readIORef (eState env)
  forM_ (sGrid s) $ \gp -> do
    let g = gpGrid gp
        top = max 0 (min (gridTotalRows g - gridVisibleRows g) (gpScroll gp + n))
    when (top /= gpScroll gp) $ modifyIORef' (eState env) $ \x -> x
      { sGrid = Just gp { gpScroll = top, gpHover = Nothing }
      , sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (gpSurface gp) (sSurfs x) }

------------------------------------------------------------------------------
-- The side panel

panelEdgeOf :: Config -> Edge
panelEdgeOf = maybe RightEdge panelEdge . sidePanel

panelAttachedOf :: Config -> Bool
panelAttachedOf = maybe False panelAttached . sidePanel

panelGap, panelPadY, stripW :: Double
panelGap = 6       -- between the panel and the edge of the screen
panelPadY = 5      -- above the first row and below the last
stripW = 1         -- the strip at the edge that opens it

-- | The strip along the edge of an output: transparent, it only takes the
-- pointer. It keeps out of the bar's exclusive zone.
createStrip :: Env -> SidePanel -> ObjectId -> IO ()
createStrip env sp o = do
  g <- sGlobals <$> readIORef st
  msf <- newSurf env o
  forM_ ((,) <$> msf <*> gLayerShell g) $ \(sf, (shell, _)) -> do
    let surf = sfSurface sf
        cfg = eCfg env
        edge = if panelEdge sp == RightEdge then zwlrLayerSurfaceV1AnchorRight else zwlrLayerSurfaceV1AnchorLeft
    ls <- zwlrLayerShellV1GetLayerSurface c shell surf o (layerOf cfg) (encodeUtf8 (namespace cfg ++ "-edge"))
    zwlrLayerSurfaceV1SetSize c ls (round stripW) 0
    zwlrLayerSurfaceV1SetAnchor c ls (edge .|. zwlrLayerSurfaceV1AnchorTop .|. zwlrLayerSurfaceV1AnchorBottom)
    zwlrLayerSurfaceV1SetExclusiveZone c ls 0
    wlSurfaceCommit c surf
    modifyIORef' st $ \s -> s
      { sSurfs = M.insert surf sf { sfW = round stripW } (sSurfs s)
      , sStrips = M.insert surf (Strip surf ls o) (sStrips s)
      , sOutputs = M.adjust (\x -> x { oStrip = Just surf }) o (sOutputs s) }
  where
    c = eConn env
    st = eState env

layerOf :: Config -> Word32
layerOf cfg = case layer cfg of
  BackgroundLayer -> zwlrLayerShellV1LayerBackground
  BottomLayer -> zwlrLayerShellV1LayerBottom
  TopLayer -> zwlrLayerShellV1LayerTop
  OverlayLayer -> zwlrLayerShellV1LayerOverlay

destroyStrip :: Env -> Strip -> IO ()
destroyStrip env t = do
  zwlrLayerSurfaceV1Destroy (eConn env) (stLayer t)
  destroySurf env (stSurface t)
  modifyIORef' (eState env) $ \s -> s
    { sStrips = M.delete (stSurface t) (sStrips s)
    , sOutputs = M.adjust (\x -> x { oStrip = Nothing }) (stOutput t) (sOutputs s) }

------------------------------------------------------------------------------
-- Hot corners

-- | A small transparent square ('hotCornerSize', 1x1 by default) in each
-- configured corner of an output. It
-- sits on top of everything in the bar's layer and only takes the pointer.
createCorners :: Env -> ObjectId -> IO ()
createCorners env o = do
  g <- sGlobals <$> readIORef st
  forM_ (gLayerShell g) $ \(shell, _) -> forM_ (nubCorners (map fst (hotCorners cfg))) $ \corner -> do
    msf <- newSurf env o
    forM_ msf $ \sf -> do
      let surf = sfSurface sf
          (v, h) = case corner of
            TopLeftCorner -> (zwlrLayerSurfaceV1AnchorTop, zwlrLayerSurfaceV1AnchorLeft)
            TopRightCorner -> (zwlrLayerSurfaceV1AnchorTop, zwlrLayerSurfaceV1AnchorRight)
            BottomLeftCorner -> (zwlrLayerSurfaceV1AnchorBottom, zwlrLayerSurfaceV1AnchorLeft)
            BottomRightCorner -> (zwlrLayerSurfaceV1AnchorBottom, zwlrLayerSurfaceV1AnchorRight)
      ls <- zwlrLayerShellV1GetLayerSurface c shell surf o zwlrLayerShellV1LayerOverlay (encodeUtf8 (namespace cfg ++ "-corner"))
      zwlrLayerSurfaceV1SetSize c ls (fromIntegral side) (fromIntegral side)
      zwlrLayerSurfaceV1SetAnchor c ls (v .|. h)
      -- in the very corner, ignoring the bar's exclusive zone
      zwlrLayerSurfaceV1SetExclusiveZone c ls (-1)
      wlSurfaceCommit c surf
      modifyIORef' st $ \s -> s
        { sSurfs = M.insert surf sf { sfW = side, sfH = side } (sSurfs s)
        , sCorners = M.insert surf (HotCorner surf ls o corner) (sCorners s) }
  modifyIORef' st $ \s -> s { sOutputs = M.adjust (\x -> x { oCorners = True }) o (sOutputs s) }
  where
    c = eConn env
    st = eState env
    cfg = eCfg env
    side = max 1 (hotCornerSize cfg)
    nubCorners = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

destroyCorner :: Env -> HotCorner -> IO ()
destroyCorner env hc = do
  zwlrLayerSurfaceV1Destroy (eConn env) (hcLayer hc)
  destroySurf env (hcSurface hc)
  modifyIORef' (eState env) $ \s -> s
    { sCorners = M.delete (hcSurface hc) (sCorners s)
    , sOutputs = M.adjust (\x -> x { oCorners = False }) (hcOutput hc) (sOutputs s) }

-- | The pointer entered or left a surface: if it is now in a hot corner,
-- run the corner's action after the delay, unless it has moved on.
cornerPointer :: Env -> IO ()
cornerPointer env = do
  s <- readIORef (eState env)
  let gen = sCornerGen s + 1
      focused = [ f | se <- M.elems (sSeats s), Just f <- [sFocus se] ]
  writeIORef (eState env) s { sCornerGen = gen }
  forM_ (listToMaybe [ f | f <- focused, M.member f (sCorners s) ]) $ \surf ->
    void $ forkIO $ threadDelay (max 0 (hotCornerDelay (eCfg env)) * 1000) >> ePush env (MsgCorner gen surf)

cornerFire :: Env -> Int -> ObjectId -> IO ()
cornerFire env gen surf = do
  s <- readIORef (eState env)
  let still = any ((== Just surf) . sFocus) (M.elems (sSeats s))
  when (gen == sCornerGen s && still) $
    forM_ (M.lookup surf (sCorners s)) $ \hc ->
      forM_ [ act | (corner, act) <- hotCorners (eCfg env), corner == hcCorner hc ] $ \act ->
        void $ forkIO $ void $ E.try @E.SomeException act

------------------------------------------------------------------------------
-- On-screen display

osdPad, osdBarH, osdGap :: Double
osdPad = 16      -- inside the box
osdBarH = 6      -- the level bar
osdGap = 10      -- between the label and the bar

-- | Follow what the OSD shows: the volume, the backlight and rivermonad's
-- keyboard layout. Only changes are shown, not the values at start-up.
startOsdSources :: OsdConfig -> R.RivermonadClient -> (Msg -> IO ()) -> IO ()
startOsdSources oc client push = do
  when (osdVolume oc) $ do
    prev <- newIORef Nothing
    onVolumeChange $ \mv -> forM_ mv $ \v -> do
      old <- atomicModifyIORef' prev (\p -> (Just v, p))
      forM_ old $ \o -> when (o /= v) $
        push (MsgOsd (osdVolumeLabel oc (volMuted v) (volPercent v))
                     (Just (if volMuted v then 0 else min 1 (fromIntegral (volPercent v) / 100))))
  when (osdBrightness oc) $ void $ forkIO $ do
    dirs <- E.try (listDirectory "/sys/class/backlight") :: IO (Either E.SomeException [FilePath])
    case dirs of
      Right (d : _) -> do
        let base = "/sys/class/backlight/" ++ d ++ "/"
            readInt f = do
              r <- E.try (readFile (base ++ f) >>= \t -> length t `seq` pure t)
              pure $ case r of
                Right t | [(n, _)] <- reads t -> Just (n :: Int)
                Right _ -> Nothing
                Left e -> const Nothing (e :: E.SomeException)
            loop old = do
              cur <- readInt "brightness"
              mx <- readInt "max_brightness"
              case (cur, mx) of
                (Just c, Just m) | m > 0, Just c /= old, isJust old -> do
                  let p = round (100 * fromIntegral c / fromIntegral m :: Double)
                  push (MsgOsd (osdBrightnessLabel oc p) (Just (fromIntegral c / fromIntegral m)))
                _ -> pure ()
              threadDelay 250000
              loop (maybe old Just cur)
        start <- readInt "brightness"
        loop start
      _ -> pure ()   -- no backlight (a desktop monitor, a VM)
  when (osdKeyboardLayout oc) $ void $ forkIO $ do
    let layoutNow = (>>= R.rsKeyboardLayout) <$> R.currentState client
        loop old = do
          threadDelay 300000
          cur <- layoutNow
          case (old, cur) of
            (Just o, Just c) | o /= c -> push (MsgOsd (osdLayoutLabel oc c) Nothing)
            _ -> pure ()
          loop (maybe old Just cur)
    layoutNow >>= loop

-- | How visible the OSD is now, eased: it fades in quickly and out a bit
-- slower.
osdAlpha :: OsdPop -> Double -> Double
osdAlpha o now = oFrom o + (oTo o - oFrom o) * (1 - (1 - p) ^ (3 :: Int))
  where
    d = osdDuration o
    p = if d <= 0 then 1 else max 0 (min 1 ((now - oStart o) / d))

osdDuration :: OsdPop -> Double
osdDuration o = if oTo o == 0 then 0.25 else 0.12

-- | Show (or update) the OSD on the focused screen and restart its timer.
osdShow :: Env -> String -> Maybe Double -> IO ()
osdShow env markup value = forM_ (osd (eCfg env)) $ \oc -> do
  s <- readIORef st
  now <- getMonotonicTime
  target <- osdOutput env
  case (sOsd s, target) of
    (Just o, Just t) | oOut o == t -> do
      let a = osdAlpha o now
          o' = o { oMarkup = markup, oValue = value, oGen = oGen o + 1
                 , oFrom = if oTo o == 1 then oFrom o else a
                 , oTo = 1, oStart = if oTo o == 1 then oStart o else now }
      modifyIORef' st $ \x -> x { sOsd = Just o'
                               , sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (oSurface o) (sSurfs x) }
      later oc (oGen o')
    (_, Just t) -> do
      closeOsd env
      gen <- maybe 0 ((+ 1) . oGen) . sOsd <$> readIORef st
      openOsd env oc t markup value gen
      later oc gen
    _ -> pure ()
  where
    st = eState env
    later oc gen = void $ forkIO $ threadDelay (osdTimeout oc * 1000) >> ePush env (MsgOsdHide gen)

-- | The output to show it on: rivermonad's focused one, else the first.
osdOutput :: Env -> IO (Maybe ObjectId)
osdOutput env = do
  s <- readIORef (eState env)
  rs <- eRiver env
  let outs = [ (o, out) | (o, out) <- M.toList (sOutputs s), oDone out ]
      focused = do
        name <- rs >>= R.rsFocusedOutput
        listToMaybe [ o | (o, out) <- outs, oName out == Just name ]
  pure $ case focused of
    Just o -> Just o
    Nothing -> fst <$> listToMaybe outs

openOsd :: Env -> OsdConfig -> ObjectId -> String -> Maybe Double -> Int -> IO ()
openOsd env oc o markup value gen = do
  s <- readIORef st
  case (gLayerShell (sGlobals s), gCompositor (sGlobals s)) of
    (Just (shell, _), Just (comp, _)) -> do
      th <- fontHeight (font cfg)
      msf <- newSurf env o
      forM_ msf $ \sf -> do
        let surf = sfSurface sf
            mg = tipMargin
            w = osdWidth oc + 2 * mg
            h = round (2 * osdPad + fromIntegral th + osdGap + osdBarH) + 2 * mg
        ls <- zwlrLayerShellV1GetLayerSurface c shell surf o zwlrLayerShellV1LayerOverlay (encodeUtf8 (namespace cfg ++ "-osd"))
        zwlrLayerSurfaceV1SetSize c ls (fromIntegral w) (fromIntegral h)
        zwlrLayerSurfaceV1SetAnchor c ls zwlrLayerSurfaceV1AnchorBottom
        zwlrLayerSurfaceV1SetMargin c ls 0 0 (fromIntegral (max 0 (osdBottom oc - mg))) 0
        zwlrLayerSurfaceV1SetExclusiveZone c ls (-1)
        -- it takes no clicks: an empty input region
        region <- wlCompositorCreateRegion c comp
        wlSurfaceSetInputRegion c surf region
        wlRegionDestroy c region
        wlSurfaceCommit c surf
        now <- getMonotonicTime
        modifyIORef' st $ \x -> x
          { sSurfs = M.insert surf sf { sfW = w, sfH = h } (sSurfs x)
          , sOsd = Just OsdPop
              { oSurface = surf, oLayer = ls, oOut = o, oMarkup = markup, oValue = value
              , oFrom = 0, oTo = 1, oStart = now, oFrame = Nothing, oGen = gen } }
    _ -> pure ()
  where
    c = eConn env
    st = eState env
    cfg = eCfg env

-- | Start fading away, if nothing was shown since the timer started.
osdHide :: Env -> Int -> IO ()
osdHide env gen = do
  s <- readIORef (eState env)
  forM_ (sOsd s) $ \o -> when (oGen o == gen && oTo o == 1) $ do
    now <- getMonotonicTime
    let o' = o { oFrom = osdAlpha o now, oTo = 0, oStart = now }
    modifyIORef' (eState env) $ \x -> x { sOsd = Just o'
                                       , sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (oSurface o) (sSurfs x) }
    -- frame callbacks stop once it is invisible: make sure it goes
    void $ forkIO $ threadDelay (round (osdDuration o' * 1000000) + 20000) >> ePush env MsgTick

-- | Close the OSD once it has faded away.
syncOsd :: Env -> IO ()
syncOsd env = do
  s <- readIORef (eState env)
  now <- getMonotonicTime
  forM_ (sOsd s) $ \o -> when (oTo o == 0 && now - oStart o >= osdDuration o) (closeOsd env)

closeOsd :: Env -> IO ()
closeOsd env = do
  s <- readIORef (eState env)
  forM_ (sOsd s) $ \o -> do
    zwlrLayerSurfaceV1Destroy (eConn env) (oLayer o)
    destroySurf env (oSurface o)
    modifyIORef' (eState env) $ \x -> x { sOsd = Nothing }

-- | The box (like a popover), the label and the level bar; it rises a few
-- pixels as it fades in.
paintOsd :: Env -> Canvas -> OsdPop -> Double -> Double -> Double -> IO ()
paintOsd env cv o alpha w h = do
  let cfg = eCfg env
      mg = fromIntegral tipMargin
      bw = w - 2 * mg
      bh = h - 2 * mg
      barColor = maybe "#cba6f7" osdBarColor (osd cfg)
  clear cv (Color 0 0 0 0)
  withFadeXY cv alpha 0 (8 * (1 - alpha)) $ do
    drawPanel cfg cv mg mg bw bh
    setColor cv (color (fgColor cfg))
    withFont (font cfg) $ \fnt -> withMarkup cv fnt (oMarkup o) $ \l -> do
      ellipsizeLayout l (bw - 2 * osdPad)
      (tw, _) <- layoutSize l
      -- centred when there is no level bar
      let tx = case oValue o of
            Nothing -> mg + (bw - fromIntegral tw) / 2
            Just _ -> mg + osdPad
          ty = case oValue o of
            Nothing -> mg + (bh - osdBarH - osdGap) / 2 - 2 + (osdBarH + osdGap) / 2 - osdPad / 2
            Just _ -> mg + osdPad
      showLayout cv l tx ty
    forM_ (oValue o) $ \v -> do
      let bx = mg + osdPad
          by = mg + bh - osdPad - osdBarH
          full = bw - 2 * osdPad
      setColor cv (color (menuHighlight cfg))
      fillRounded cv bx by full osdBarH (osdBarH / 2)
      when (v > 0) $ do
        setColor cv (color barColor)
        fillRounded cv bx by (max osdBarH (full * max 0 (min 1 v))) osdBarH (osdBarH / 2)

-- | When an output goes away.
destroyPanelOn :: Env -> ObjectId -> IO ()
destroyPanelOn env o = do
  s <- readIORef (eState env)
  mapM_ (destroyStrip env) [ t | t <- M.elems (sStrips s), stOutput t == o ]
  mapM_ (destroyCorner env) [ hc | hc <- M.elems (sCorners s), hcOutput hc == o ]
  forM_ (sOsd s) $ \x -> when (oOut x == o) (closeOsd env)
  forM_ (sPanel s) $ \pl -> when (plOutput pl == o) (closePanel env)

-- | The pointer entered or left one of our surfaces: open the panel after
-- a moment at the edge, keep it while the pointer is on it, and close it
-- a moment after the pointer has left.
panelPointer :: Env -> IO ()
panelPointer env = forM_ (sidePanel (eCfg env)) $ \sp -> do
  s <- readIORef (eState env)
  let gen = sPanelGen s + 1
      focused = [ f | se <- M.elems (sSeats s), Just f <- [sFocus se] ]
      onStrip = any (`M.member` sStrips s) focused
      onPanel = any (\f -> Just f == (plSurface <$> sPanel s)) focused
      later ms msg = void $ forkIO $ threadDelay (max 0 ms * 1000) >> ePush env msg
  writeIORef (eState env) s { sPanelGen = gen }
  case sPanel s of
    Just pl
      | onPanel || onStrip -> when (plTo pl == 0) (panelOpen env gen)
      | plTo pl == 1 -> later (panelHideDelay sp) (MsgPanelHide gen)
      | otherwise -> pure ()
    Nothing
      | onStrip -> if panelDelay sp <= 0 then panelOpen env gen else later (panelDelay sp) (MsgPanelOpen gen)
      | otherwise -> pure ()

-- | Open the panel (or bring it back while it slides away) on the output
-- whose strip the pointer is on, if nothing happened since @gen@.
panelOpen :: Env -> Int -> IO ()
panelOpen env gen = do
  s <- readIORef (eState env)
  let focused = [ f | se <- M.elems (sSeats s), Just f <- [sFocus se] ]
      strip = listToMaybe [ t | f <- focused, Just t <- [M.lookup f (sStrips s)] ]
      onPanel = any (\f -> Just f == (plSurface <$> sPanel s)) focused
  when (gen == sPanelGen s) $ case (sPanel s, strip) of
    (Just pl, _) | onPanel || maybe False ((== plOutput pl) . stOutput) strip -> when (plTo pl == 0) $ do
      now <- getMonotonicTime
      setPanel env pl { plFrom = panelAlpha (eCfg env) pl now, plTo = 1, plStart = now }
    (Just _, Just t) -> closePanel env >> openPanel env (stOutput t)
    (Nothing, Just t) -> openPanel env (stOutput t)
    _ -> pure ()

-- | Slide the panel away, if the pointer has not come back since @gen@;
-- not while one of its menus is open.
panelHide :: Env -> Int -> IO ()
panelHide env gen = do
  s <- readIORef (eState env)
  forM_ (sPanel s) $ \pl -> when (gen == sPanelGen s && plTo pl == 1) $
    if maybe False ((== plSurface pl) . pBar) (sPopup s)
      then void $ forkIO $ threadDelay 300000 >> ePush env (MsgPanelHide gen)
      else do
        now <- getMonotonicTime
        let pl' = pl { plFrom = panelAlpha (eCfg env) pl now, plTo = 0, plStart = now }
        setPanel env pl'
        -- frame callbacks stop if the surface is hidden: make sure it goes
        void $ forkIO $ threadDelay (round (panelDuration (eCfg env) pl' * 1000000) + 20000) >> ePush env MsgTick

setPanel :: Env -> Panel -> IO ()
setPanel env pl = modifyIORef' (eState env) $ \s -> s
  { sPanel = Just pl, sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (plSurface pl) (sSurfs s) }

-- | Close the panel once it has slid away.
syncPanel :: Env -> IO ()
syncPanel env = do
  s <- readIORef (eState env)
  now <- getMonotonicTime
  forM_ (sPanel s) $ \pl ->
    when (plTo pl == 0 && now - plStart pl >= panelDuration (eCfg env) pl) (closePanel env)

-- | How far out the panel is now (0 hidden, 1 out), eased.
panelAlpha :: Config -> Panel -> Double -> Double
panelAlpha cfg pl now = plFrom pl + (plTo pl - plFrom pl) * (1 - (1 - p) ^ (3 :: Int))
  where
    d = panelDuration cfg pl
    p = if d <= 0 then 1 else max 0 (min 1 ((now - plStart pl) / d))

-- | Seconds the slide takes; going away is quicker than coming out.
panelDuration :: Config -> Panel -> Double
panelDuration cfg pl = fromIntegral (max 0 (maybe 0 panelSlide (sidePanel cfg))) / 1000 * (if plTo pl == 0 then 0.7 else 1)

-- | The visible blocks of the panel's widgets, one per row.
panelBlocks :: Env -> Output -> IO [Block]
panelBlocks env out = concat <$> mapM get (ePanel env)
  where
    get f = filter visible <$> f out `E.catch` \(e :: E.SomeException) -> do
      hPutStrLn stderr ("waymobar: widget failed: " ++ show e)
      pure []
    visible b = not (null (blockMarkup b)) || isJust (blockIcon b) || isJust (blockIconSource b)

-- | Inner padding of a row: the block's own, or a menu-like default.
rowPad :: Block -> Double
rowPad b = let p = fromIntegral (blockPadding b) + boxPadding (blockStyle b) in if p > 0 then p else 12

rowIconSize :: Env -> Block -> Double
rowIconSize env b = fromIntegral (if blockIconSize b > 0 then blockIconSize b else max 8 (eHeight env - 8))

-- | Width and height of a row with this block and its laid out markup.
rowSize :: Env -> Block -> (Int, Int) -> (Double, Double)
rowSize env b (tw, th) = (w, rowH)
  where
    fixed = maybe 0 panelRowHeight (sidePanel (eCfg env))
    rowH
      | fixed > 0 = max (fromIntegral fixed) (fromIntegral th)
      | otherwise = max (fromIntegral (eHeight env)) (fromIntegral th + 12)
    hasIcon = isJust (blockIcon b) || isJust (blockIconSource b)
    iconPart
      | hasIcon = rowIconSize env b + (if null (blockMarkup b) then 0 else 4)
      | otherwise = 0
    textW = if null (blockMarkup b) then 0 else fromIntegral tw
    (_, mh) = boxMargin (blockStyle b)
    w = max (fromIntegral (blockMinWidth b)) (iconPart + textW + 2 * rowPad b) + 2 * mh

-- | The panel's width and height (without the room for its shadow).
measurePanel :: Env -> [Block] -> IO (Double, Double)
measurePanel env blocks =
  allocaBytes 4 $ \p -> withCanvas p 1 1 1 $ \cv -> withFont (font (eCfg env)) $ \fnt ->
    withLayouts cv fnt (map blockMarkup blocks) $ \lays -> do
      sizes <- zipWithM (\b l -> rowSize env b <$> layoutSize l) blocks lays
      pure ( maximum (0 : map fst sizes) + 2 * (menuBorderW + menuInset)
           , sum (map snd sizes) + 2 * (menuBorderW + panelPadY) )

-- | Slide the panel out on an output.
openPanel :: Env -> ObjectId -> IO ()
openPanel env o = do
  s <- readIORef st
  let g = sGlobals s
      cfg = eCfg env
      name = maybe "" (fromMaybe "" . oName) (M.lookup o (sOutputs s))
  blocks <- panelBlocks env name
  case (gLayerShell g, gCompositor g) of
    (Just (shell, _), Just (comp, _)) | not (null blocks) -> do
      (pw, ph) <- measurePanel env blocks
      msf <- newSurf env o
      forM_ msf $ \sf -> do
        let surf = sfSurface sf
            mg = fromIntegral tipMargin
            right = panelEdgeOf cfg == RightEdge
            -- attached, it touches the edge; floating, it keeps a gap
            gap = if panelAttachedOf cfg then 0 else panelGap
            w = ceiling (pw + mg + gap) :: Int
            h = ceiling (ph + 2 * mg) :: Int
            px = if right then mg else gap
            -- the scale of the strip is the best guess until the compositor says
            stripSf = listToMaybe [ x | t <- M.elems (sStrips s), stOutput t == o, Just x <- [M.lookup (stSurface t) (sSurfs s)] ]
        ls <- zwlrLayerShellV1GetLayerSurface c shell surf o (layerOf cfg) (encodeUtf8 (namespace cfg ++ "-panel"))
        zwlrLayerSurfaceV1SetSize c ls (fromIntegral w) (fromIntegral h)
        -- anchored to the edge only: centred along it
        zwlrLayerSurfaceV1SetAnchor c ls (if right then zwlrLayerSurfaceV1AnchorRight else zwlrLayerSurfaceV1AnchorLeft)
        zwlrLayerSurfaceV1SetExclusiveZone c ls 0
        -- the panel takes the pointer, and so does the room between it and
        -- the edge, so the pointer can still rest there
        region <- wlCompositorCreateRegion c comp
        if right
          then wlRegionAdd c region (round px) (round mg) (fromIntegral w - round px) (round ph)
          else wlRegionAdd c region 0 (round mg) (round (px + pw)) (round ph)
        wlSurfaceSetInputRegion c surf region
        wlRegionDestroy c region
        wlSurfaceCommit c surf
        now <- getMonotonicTime
        modifyIORef' st $ \s' -> s'
          { sSurfs = M.insert surf sf { sfW = w, sfH = h
                                      , sfScale120 = maybe 0 sfScale120 stripSf
                                      , sfBufScale = maybe 0 sfBufScale stripSf } (sSurfs s')
          , sPanel = Just Panel
              { plSurface = surf, plLayer = ls, plOutput = o, plOutputName = name
              , plBox = (px, px + pw), plHits = [], plRows = [], plTips = [], plHoverY = Nothing
              , plFrom = 0, plTo = 1, plStart = now, plFrame = Nothing } }
    _ -> pure ()
  where
    c = eConn env
    st = eState env

closePanel :: Env -> IO ()
closePanel env = do
  s <- readIORef (eState env)
  forM_ (sPanel s) $ \pl -> do
    forM_ (sPopup s) $ \p -> when (pBar p == plSurface pl) (closeMenu env)
    forM_ (sTip s) $ \t -> when (tBar t == plSurface pl) (closeTip env)
    zwlrLayerSurfaceV1Destroy (eConn env) (plLayer pl)
    destroySurf env (plSurface pl)
    modifyIORef' (eState env) $ \s' -> s' { sPanel = Nothing }

-- | Draw the panel, @alpha@ of the way out: it slides from the edge while
-- it fades in. Returns the rows that take clicks and those with a hover
-- look (y ranges, where they are once it is out).
paintPanel :: Env -> Canvas -> Panel -> Double -> Double -> Double
           -> IO ([(Double, Double, Click -> IO ())], [(Double, Double)], [(Double, Double, String)])
paintPanel env cv pl alpha w h = do
  let cfg = eCfg env
      mg = fromIntegral tipMargin
      (px, px1) = plBox pl
      pw = px1 - px
      right = panelEdgeOf cfg == RightEdge
      attached = panelAttachedOf cfg
      away = if right then 1 else -1
      travel = (pw + (if attached then 0 else panelGap)) * (1 - alpha) * away
      -- attached: a tab that grows out of the edge, its contents coming
      -- out with it; floating: the panel of a menu, sliding in as it fades
      frame body
        | attached = do
            let edgeX = if right then w else 0
                depth = pw * alpha
                r = min (popoverRadius cfg) mg
            setColor cv (color (fromMaybe (bgColor cfg) (popoverBg cfg)))
            fillEdgeTab cv edgeX (negate away) depth mg (h - mg) r
            x <- withEdgeTabClip cv edgeX (negate away) depth mg (h - mg) r (withTranslate cv travel 0 body)
            setColor cv (color (menuBorder cfg))
            strokeEdgeTab cv edgeX (negate away) depth mg (h - mg) r 1
            pure x
        | otherwise = withFadeXY cv (min 1 (alpha * 1.4)) travel 0 $
            drawPanel cfg cv px mg pw (h - 2 * mg) >> body
      rx = px + menuBorderW + menuInset
      rw = pw - 2 * (menuBorderW + menuInset)
  blocks <- panelBlocks env (plOutputName pl)
  devScale <- canvasScale cv
  clear cv (Color 0 0 0 0)
  frame $
    withFont (font cfg) $ \fnt -> withLayouts cv fnt (map blockMarkup blocks) $ \lays -> do
      sizes <- mapM layoutSize lays
      let heights = [ snd (rowSize env b sz) | (b, sz) <- zip blocks sizes ]
          ys = scanl (+) (mg + menuBorderW + panelPadY) heights
      rows <- forM (zip4 blocks lays sizes (zip ys (drop 1 ys))) $ \(b, l, (_, th), (y0, y1)) -> do
        let clickable = isJust (blockOnClick b)
            base = blockStyle b
            -- clickable rows light up like the entries of a menu
            hov = case styleHover base of
              Just hs -> Just hs
              Nothing | clickable -> Just noStyle
                { styleBg = Just (menuHighlight cfg), styleRadius = Just (max 0 (popoverRadius cfg - menuInset)) }
              Nothing -> Nothing
            hovered = maybe False (\y -> y >= y0 && y < y1) (plHoverY pl)
            st = if hovered then maybe base (`mergeStyle` base) hov else base
            isz = rowIconSize env b
        ic <- case blockIconSource b of
          Nothing -> pure (blockIcon b)
          Just src -> do
            let Color r g bl _ = color (fromMaybe (fromMaybe (fgColor cfg) (styleFg st)) (blockIconColor b))
            maybe (blockIcon b) Just <$> resolveIcon src (max 1 (round (isz * devScale))) (Just (r, g, bl)) (blockIconRecolorAll b)
        withTranslate cv 0 y0 $ void $
          drawBlock cfg cv b { blockStyle = st, blockIcon = ic, blockPadding = round (rowPad b) - round (boxPadding st) }
            l rx rw (y1 - y0) th isz
        pure ( [ (y0, y1, f) | Just f <- [blockOnClick b] ], [ (y0, y1) | isJust hov ]
             , [ (y0, y1, m) | Just m <- [blockHoverMarkup b], not (null m) ] )
      pure (concat [ a | (a, _, _) <- rows ], concat [ b | (_, b, _) <- rows ], concat [ t | (_, _, t) <- rows ])

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
        panelPointer env
        cornerPointer env
      WlPointerLeave _ _ -> do
        setSeat se { sFocus = Nothing, sAxis = 0 }
        hover Nothing 0 0
        panelPointer env
        cornerPointer env
        s' <- readIORef st
        forM_ (sPopup s') $ \pop -> do
          let gen = pAway pop + 1
          modifyIORef' st $ \x -> x { sPopup = Just pop { pAway = gen } }
          void $ forkIO $ threadDelay 700000 >> ePush env (MsgMenuTimeout gen)
        forM_ (sGrid s') $ \gp -> do
          let gen = gpAway gp + 1
          modifyIORef' st $ \x -> x { sGrid = Just gp { gpAway = gen } }
          void $ forkIO $ threadDelay 700000 >> ePush env (MsgGridTimeout gen)
      WlPointerMotion _ x y -> do
        setSeat se { sX = x, sY = y }
        hover (sFocus se) x y
      WlPointerButton serial _ btn state | state == wlPointerButtonStatePressed -> do
        modifyIORef' st $ \s' -> s' { sLastPress = Just (seat, serial) }
        case (sGrid s, sPopup s) of
          (Just gp, _) | sFocus se == Just (gpSurface gp) ->
            forM_ (gridIndexAt gp (sX se) (sY se) >>= \i -> listToMaybe (drop i (gridEntries (gpGrid gp)))) $ \e -> do
              closeGrid env
              void $ forkIO $ void $ E.try @E.SomeException (gridAction e)
          (Just gp, _) -> do
            -- a click elsewhere closes it; on the block it opened from that
            -- is all (so the block toggles it)
            closeGrid env
            let (a0, a1) = gpArea gp
                onOpener = sFocus se == Just (gpBar gp) && sX se >= a0 && sX se < a1
            unless onOpener $ click se (button btn)
          (Nothing, mpop) -> case mpop of
            Just pop | sFocus se == Just (pSurface pop) ->
              forM_ (rowAt pop (sY se)) (activateRow env pop)
            Just pop -> do
              -- a click on the bar closes the menu; on another block it also
              -- does what it normally does
              closeMenu env
              let (a0, a1) = pArea pop
                  -- along a bar the area is an x range, beside the side panel a y range
                  at = if M.member (pBar pop) (sBars s) then sX se else sY se
                  onOpener = sFocus se == Just (pBar pop) && at >= a0 && at < a1
              unless onOpener $ click se (button btn)
            Nothing -> click se (button btn)
      _ -> pointerRest s se setSeat
  where
    button :: Word32 -> Button
    button = \case
      0x110 -> LeftButton
      0x111 -> RightButton
      0x112 -> MiddleButton
      n -> OtherButton (fromIntegral n)
    pointerRest s0 se setSeat = case pe of
      WlPointerAxis _ axis v | axis == wlPointerAxisVerticalScroll, Just gp <- onGrid s0 se -> do
        -- one row per notch of the wheel (about 10 units)
        let acc = gpAxis gp + v
            n = truncate (acc / 10) :: Int
        modifyIORef' st $ \x -> x { sGrid = Just gp { gpAxis = acc - fromIntegral n * 10 } }
        when (n /= 0) (scrollGrid env n)
      WlPointerAxis _ axis v | axis == wlPointerAxisVerticalScroll -> do
        let acc = sAxis se + v
        if abs acc >= 10
          then do
            setSeat se { sAxis = 0 }
            click se (if acc > 0 then ScrollDown else ScrollUp)
          else setSeat se { sAxis = acc }
      WlPointerAxisStop _ axis | axis == wlPointerAxisVerticalScroll -> setSeat se { sAxis = 0 }
      _ -> pure ()
    onGrid s0 se = do
      gp <- sGrid s0
      guard (sFocus se == Just (gpSurface gp))
      pure gp
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
      forM_ (sPanel s) $ \pl -> do
        let hy = if focus == Just (plSurface pl) && inBox pl x then Just y else Nothing
            under = \py -> [ r | r@(y0, y1) <- plRows pl, Just p' <- [py], p' >= y0, p' < y1 ]
        when (hy /= plHoverY pl) $ modifyIORef' st $ \s' -> s'
          { sPanel = Just pl { plHoverY = hy }
          , sSurfs = if under hy /= under (plHoverY pl) then M.adjust (\sf -> sf { sfDirty = True }) (plSurface pl) (sSurfs s') else sSurfs s' }
      forM_ (sGrid s) $ \gp -> do
        let h = if focus == Just (gpSurface gp) then gridIndexAt gp x y else Nothing
        when (h /= gpHover gp) $ modifyIORef' st $ \s' -> s'
          { sGrid = Just gp { gpHover = h }
          , sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (gpSurface gp) (sSurfs s') }
      forM_ (sPopup s) $ \pop -> do
        let h = if focus == Just (pSurface pop) then rowAt pop y else Nothing
        when (h /= pHover pop) $ modifyIORef' st $ \s' -> s'
          { sPopup = Just pop { pHover = h }
          , sSurfs = M.adjust (\sf -> sf { sfDirty = True }) (pSurface pop) (sSurfs s') }
    inBox pl x = let (x0, x1) = plBox pl in x >= x0 && x < x1
    click se btn = do
      s <- readIORef st
      forM_ (sPanel s) $ \pl -> when (sFocus se == Just (plSurface pl) && inBox pl (sX se)) $
        forM_ [ (y0, y1, f) | (y0, y1, f) <- plHits pl, sY se >= y0, sY se < y1 ] $ \(y0, y1, f) ->
          void $ forkIO $ void $ E.try @E.SomeException $ f Click
            { clickButton = btn
            , clickOutput = plOutputName pl
            , clickArea = plBox pl
            , clickMenu = ePush env . MsgMenu (plSurface pl) (y0, y1)
            , clickGrid = const (pure ())   -- grids open under a bar
            }
      forM_ (sFocus se >>= \surf -> (,) surf <$> M.lookup surf (sBars s)) $ \(surf, b) ->
        forM_ [ (x0, x1, f) | (x0, x1, f) <- bHits b, sX se >= x0, sX se < x1 ] $ \(x0, x1, f) ->
          void $ forkIO $ void $ E.try @E.SomeException $ f Click
            { clickButton = btn
            , clickOutput = bOutputName b
            , clickArea = (x0, x1)
            , clickMenu = ePush env . MsgMenu surf (x0, x1)
            , clickGrid = ePush env . MsgGrid surf (x0, x1)
            }

------------------------------------------------------------------------------

utf8 :: B.ByteString -> String
utf8 = T.unpack . TE.decodeUtf8Lenient

encodeUtf8 :: String -> B.ByteString
encodeUtf8 = TE.encodeUtf8 . T.pack
