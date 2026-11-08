-- | The network connection: the interface of the default route, its IPv4
-- address and traffic, and (through NetworkManager on the system bus, when
-- it is running) the connection name, Wi-Fi network and signal.
module Waymobar.Network
  ( NetInfo (..)
  , NetKind (..)
  , network
  , networkInfo
  , formatRate
  ) where

import qualified Control.Exception as E
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Numeric (showFFloat)
import System.Directory (doesDirectoryExist)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.ByteString as B

import Waymobar.Core
import Waymobar.DBus
import Waymobar.Widgets (periodicIO)

foreign import ccall unsafe "waymobar_ipv4" c_ipv4 :: CString -> CString -> CInt -> IO CInt

data NetKind = Wifi | Ethernet | OtherNet
  deriving (Eq, Show)

data NetInfo = NetInfo
  { netInterface :: String        -- ^ e.g. "wlan0", "enp0s1"
  , netKind      :: NetKind
  , netName      :: Maybe String  -- ^ the Wi-Fi network, or NetworkManager's connection name
  , netSignal    :: Maybe Int     -- ^ Wi-Fi signal, percent
  , netAddress   :: Maybe String  -- ^ IPv4 address
  , netRxRate    :: Double        -- ^ bytes per second received
  , netTxRate    :: Double        -- ^ bytes per second sent
  } deriving (Eq, Show)

-- | The connection, every @secs@ seconds; 'Nothing' when offline. Return ""
-- to hide the widget.
--
-- > network 3 $ \case
-- >   Nothing -> "offline"
-- >   Just n | netKind n == Wifi -> maybe "wifi" escape (netName n) ++ maybe "" (\s -> " " ++ show s ++ "%") (netSignal n)
-- >          | otherwise -> "eth " ++ maybe "" id (netAddress n)
network :: Double -> (Maybe NetInfo -> String) -> Widget
network secs fmt = periodicIO secs $ do
  prev <- newIORef Nothing
  pure (fmt <$> networkInfo prev)

-- | Sample the network now. The IORef keeps the previous byte counters to
-- compute rates.
networkInfo :: IORef (Maybe (String, Integer, Integer, UTCTime)) -> IO (Maybe NetInfo)
networkInfo prev = do
  miface <- defaultInterface
  case miface of
    Nothing -> writeIORef prev Nothing >> pure Nothing
    Just iface -> do
      let sys f = "/sys/class/net/" ++ iface ++ "/" ++ f
      up <- (\s -> trim s /= "down") <$> readOr "" (sys "operstate")
      if not up then pure Nothing else do
        wireless <- doesDirectoryExist (sys "wireless")
        typ <- trim <$> readOr "" (sys "type")
        rx <- readNum (sys "statistics/rx_bytes")
        tx <- readNum (sys "statistics/tx_bytes")
        now <- getCurrentTime
        p <- readIORef prev
        writeIORef prev (Just (iface, rx, tx, now))
        let (rxr, txr) = case p of
              Just (i, rx0, tx0, t0) | i == iface, dt > 0 ->
                (fromIntegral (rx - rx0) / dt, fromIntegral (tx - tx0) / dt)
                where dt = realToFrac (diffUTCTime now t0) :: Double
              _ -> (0, 0)
        addr <- ipv4 iface
        nm <- nmInfo iface
        sig <- case nm of
          Just (_, Just s) -> pure (Just s)
          _ | wireless -> procWireless iface
            | otherwise -> pure Nothing
        pure $ Just NetInfo
          { netInterface = iface
          , netKind = if wireless then Wifi else if typ == "1" then Ethernet else OtherNet
          , netName = fst =<< nm
          , netSignal = sig
          , netAddress = addr
          , netRxRate = max 0 rxr
          , netTxRate = max 0 txr
          }

-- | "1.2 MB/s", "340 kB/s", "12 B/s".
formatRate :: Double -> String
formatRate r
  | r >= 1e9 = showFFloat (Just 1) (r / 1e9) " GB/s"
  | r >= 1e6 = showFFloat (Just 1) (r / 1e6) " MB/s"
  | r >= 1e3 = show (round (r / 1e3) :: Int) ++ " kB/s"
  | otherwise = show (round r :: Int) ++ " B/s"

-- | The interface of the default IPv4 route with the lowest metric.
defaultInterface :: IO (Maybe String)
defaultInterface = do
  s <- readOr "" "/proc/net/route"
  let routes = [ (metric, iface)
               | (iface : dest : _gw : flags : _ : _ : m : _) <- map words (drop 1 (lines s))
               , dest == "00000000"
               , Just f <- [readHex' flags], odd f   -- RTF_UP
               , Just metric <- [readMaybe' m :: Maybe Int] ]
  pure (snd <$> listToMaybe (sortOn fst routes))
  where
    readHex' x = case reads ("0x" ++ x) :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing

ipv4 :: String -> IO (Maybe String)
ipv4 iface = withCString iface $ \ci -> allocaBytes 64 $ \out -> do
  ok <- c_ipv4 ci out 64
  if ok == 0 then pure Nothing else Just <$> peekCString out

-- | Link quality from /proc/net/wireless (out of 70), as a percentage.
procWireless :: String -> IO (Maybe Int)
procWireless iface = do
  s <- readOr "" "/proc/net/wireless"
  pure $ listToMaybe
    [ min 100 (round (q * 100 / 70))
    | (name : _status : quality : _) <- map words (drop 2 (lines s))
    , takeWhile (/= ':') name == iface
    , Just q <- [readMaybe' (takeWhile (/= '.') quality) :: Maybe Double] ]

------------------------------------------------------------------------------
-- NetworkManager

{-# NOINLINE systemBus #-}
systemBus :: IORef (Maybe (Maybe Bus))
systemBus = unsafePerformIO (newIORef Nothing)

-- | The system bus, connected once (Nothing if unavailable).
getSystemBus :: IO (Maybe Bus)
getSystemBus = readIORef systemBus >>= \case
  Just b -> pure b
  Nothing -> do
    r <- E.try @E.SomeException connectSystem
    let b = either (const Nothing) Just r
    writeIORef systemBus (Just b)
    pure b

nm :: String
nm = "org.freedesktop.NetworkManager"

-- | Connection name (the SSID for Wi-Fi) and Wi-Fi strength.
nmInfo :: String -> IO (Maybe (Maybe String, Maybe Int))
nmInfo iface = getSystemBus >>= \case
  Nothing -> pure Nothing
  Just bus -> do
    let prop obj iface' name = do
          r <- call bus nm obj "org.freedesktop.DBus.Properties" "Get" [VString iface', VString name]
          pure $ case r of
            Right [v] -> Just (fromVariant v)
            _ -> Nothing
    dev <- call bus nm "/org/freedesktop/NetworkManager" nm "GetDeviceByIpIface" [VString iface]
    case dev of
      Right [VObjectPath d] -> do
        ac <- prop d (nm ++ ".Device") "ActiveConnection"
        name <- case ac of
          Just (VObjectPath a) | a /= "/" -> (>>= valueString) <$> prop a (nm ++ ".Connection.Active") "Id"
          _ -> pure Nothing
        ap <- prop d (nm ++ ".Device.Wireless") "ActiveAccessPoint"
        (ssid, strength) <- case ap of
          Just (VObjectPath a) | a /= "/" -> do
            s <- prop a (nm ++ ".AccessPoint") "Ssid"
            st <- prop a (nm ++ ".AccessPoint") "Strength"
            pure ( case s of
                     Just (VBytes bs) | not (B.null bs) -> Just (T.unpack (TE.decodeUtf8Lenient bs))
                     _ -> Nothing
                 , st >>= valueInt )
          _ -> pure (Nothing, Nothing)
        pure (Just (maybe name Just ssid, strength))
      _ -> pure Nothing

------------------------------------------------------------------------------

readOr :: String -> FilePath -> IO String
readOr d p = do
  r <- E.try @E.IOException $ do
    s <- readFile p
    length s `seq` pure s
  pure (either (const d) id r)

readNum :: FilePath -> IO Integer
readNum p = maybe 0 id . readMaybe' . trim <$> readOr "0" p

readMaybe' :: Read a => String -> Maybe a
readMaybe' s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
