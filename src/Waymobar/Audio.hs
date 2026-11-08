-- | Volume of the default output, through libpulse: works with PipeWire
-- (pipewire-pulse) and PulseAudio, follows changes as they happen (no
-- polling). Without a PulseAudio server it falls back to polling WirePlumber
-- with @wpctl@.
module Waymobar.Audio
  ( Volume (..)
  , volume
  , volumeWith
  , VolumeConfig (..)
  , defaultVolume
  , toggleMute
  , changeVolume
  , currentVolume
  ) where

import Control.Concurrent (forkIO, threadDelay, threadWaitRead)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import Data.List (isPrefixOf)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO.ByteString (fdRead)
import System.Posix.Types (Fd (..))
import Text.Read (readMaybe)

import Waymobar.Core
import Waymobar.Widgets (readCommand, spawn)

foreign import ccall unsafe "waymobar_pulse_start" c_start :: IO CInt
foreign import ccall unsafe "waymobar_pulse_get" c_get :: Ptr CInt -> Ptr CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe "waymobar_pulse_toggle_mute" c_toggle :: IO CInt
foreign import ccall unsafe "waymobar_pulse_change" c_change :: CInt -> CInt -> IO CInt

-- | The default output.
data Volume = Volume
  { volPercent :: Int     -- ^ 100 = nominal; can go above when boosted
  , volMuted   :: Bool
  , volDevice  :: String  -- ^ e.g. "Built-in Audio Analog Stereo" (empty with wpctl)
  } deriving (Eq, Show)

data VolumeConfig = VolumeConfig
  { volStep :: Int  -- ^ percent per wheel step
  , volMax  :: Int  -- ^ the wheel never goes above this
  }

defaultVolume :: VolumeConfig
defaultVolume = VolumeConfig 5 100

-- | The volume of the default output: left click mutes, the wheel changes
-- it. 'Nothing' when there is no output (the widget then shows whatever
-- the function returns for it; return "" to hide it).
volume :: (Maybe Volume -> String) -> Widget
volume = volumeWith defaultVolume

volumeWith :: VolumeConfig -> (Maybe Volume -> String) -> Widget
volumeWith cfg fmt = Widget $ \env -> do
  ref <- newIORef Nothing
  ensureStarted
  addListener $ \v -> do
    old <- atomicModifyIORef' ref (\o -> (v, o))
    when (old /= v) (redraw env)
  readIORef current >>= writeIORef ref
  pure $ \_ -> do
    v <- readIORef ref
    let m = fmt v
    pure [ (block m) { blockOnClick = Just click } | not (null m) ]
  where
    click c = case clickButton c of
      LeftButton -> toggleMute
      ScrollUp -> changeVolume (volStep cfg) (volMax cfg)
      ScrollDown -> changeVolume (negate (volStep cfg)) (volMax cfg)
      _ -> pure ()

------------------------------------------------------------------------------
-- One backend per process, shared by every volume widget

{-# NOINLINE current #-}
current :: IORef (Maybe Volume)
current = unsafePerformIO (newIORef Nothing)

{-# NOINLINE listeners #-}
listeners :: IORef [Maybe Volume -> IO ()]
listeners = unsafePerformIO (newIORef [])

{-# NOINLINE started #-}
started :: MVar Bool
started = unsafePerformIO (newMVar False)

{-# NOINLINE usePulse #-}
usePulse :: IORef Bool
usePulse = unsafePerformIO (newIORef True)

addListener :: (Maybe Volume -> IO ()) -> IO ()
addListener l = atomicModifyIORef' listeners (\ls -> (l : ls, ()))

publish :: Maybe Volume -> IO ()
publish v = do
  writeIORef current v
  readIORef listeners >>= mapM_ ($ v)

-- | The last known volume.
currentVolume :: IO (Maybe Volume)
currentVolume = readIORef current

ensureStarted :: IO ()
ensureStarted = modifyMVar_ started $ \s -> do
  unless s $ do
    fd <- c_start
    if fd < 0
      then writeIORef usePulse False >> void (forkIO wpctlLoop)
      else do
        void $ forkIO $ forever $ do
          threadWaitRead (Fd fd)
          _ <- E.try @E.SomeException (fdRead (Fd fd) 8)
          readPulse >>= \case
            Just v -> writeIORef usePulse True >> publish (Just v)
            -- while polling wpctl, a failed reconnection says nothing new
            Nothing -> readIORef usePulse >>= \p -> when p (publish Nothing)
        -- no PulseAudio server after a while: poll wpctl until there is one
        void $ forkIO $ do
          threadDelay 3000000
          v <- readPulse
          when (v == Nothing) $ do
            writeIORef usePulse False
            wpctlLoop
  pure True

readPulse :: IO (Maybe Volume)
readPulse =
  alloca $ \pv -> alloca $ \pm -> allocaBytes 256 $ \desc -> do
    ok <- c_get pv pm desc 256
    if ok == 0
      then pure Nothing
      else do
        v <- peek pv
        m <- peek pm
        d <- peekCString desc
        pure (Just (Volume (fromIntegral v) (m /= 0) d))

-- | Fallback without a PulseAudio server: ask WirePlumber every 2 seconds,
-- until libpulse connects.
wpctlLoop :: IO ()
wpctlLoop = do
  pulse <- readPulse
  case pulse of
    Just v -> writeIORef usePulse True >> publish (Just v)  -- libpulse took over
    Nothing -> do
      writeIORef usePulse False
      out <- E.try @E.SomeException (readCommand "wpctl get-volume @DEFAULT_AUDIO_SINK@")
      publish $ case words <$> out of
        Right ("Volume:" : x : rest) | Just f <- (readMaybe x :: Maybe Double) ->
          Just (Volume (round (f * 100)) (any ("[MUTED]" `isPrefixOf`) rest) "")
        _ -> Nothing
      threadDelay 2000000
      wpctlLoop

toggleMute :: IO ()
toggleMute = do
  p <- readIORef usePulse
  ok <- if p then (/= 0) <$> c_toggle else pure False
  unless ok $ spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

-- | Change the volume by a number of percent (negative to lower it),
-- capped at the given maximum.
changeVolume :: Int -> Int -> IO ()
changeVolume delta cap = do
  p <- readIORef usePulse
  ok <- if p then (/= 0) <$> c_change (fromIntegral delta) (fromIntegral cap) else pure False
  unless ok $ spawn $
    if delta >= 0
      then "wpctl set-volume -l " ++ show (fromIntegral cap / 100 :: Double) ++ " @DEFAULT_AUDIO_SINK@ " ++ show delta ++ "%+"
      else "wpctl set-volume @DEFAULT_AUDIO_SINK@ " ++ show (negate delta) ++ "%-"
