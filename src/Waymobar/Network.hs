-- | The network connection: the interface of the default route, its IPv4
-- address and traffic, and (through NetworkManager on the system bus, when
-- it is running) the connection name, Wi-Fi network and signal.
module Waymobar.Network
  ( NetInfo (..)
  , NetKind (..)
  , network
  , networkHover
  , networkDetails
  , wifiMenu
  , networkInfo
  , formatRate
  ) where

import qualified Control.Exception as E
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.Char (digitToInt, isHexDigit, isSpace)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef
import Data.List (intercalate, nub, sortOn)
import Data.Maybe (listToMaybe)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Numeric (showFFloat)
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.ByteString as B

import Waymobar.Core
import Waymobar.DBus
import Waymobar.Draw (escapeMarkup)
import Waymobar.Widgets (onClick, periodicHoverIO, periodicIO)

foreign import ccall unsafe "waymobar_ipv4" c_ipv4 :: CString -> CString -> CInt -> IO CInt
foreign import ccall unsafe "waymobar_ipv6" c_ipv6 :: CString -> CString -> CInt -> IO CInt

data NetKind = Wifi | Ethernet | OtherNet
  deriving (Eq, Show)

data NetInfo = NetInfo
  { netInterface :: String        -- ^ e.g. "wlan0", "enp0s1"
  , netKind      :: NetKind
  , netName      :: Maybe String  -- ^ the Wi-Fi network, or NetworkManager's connection name
  , netSignal    :: Maybe Int     -- ^ Wi-Fi signal, percent
  , netAddress   :: Maybe String  -- ^ IPv4 address
  , netAddress6  :: Maybe String  -- ^ IPv6 address (not the link-local one)
  , netGateway   :: Maybe String  -- ^ the router (IPv4, or IPv6 when there is no IPv4 route)
  , netFrequency :: Maybe Int     -- ^ Wi-Fi frequency, MHz (NetworkManager)
  , netBitrate   :: Maybe Int     -- ^ Wi-Fi link speed, Mbit/s (NetworkManager)
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

-- | Like 'network', with a second function for the markup of the popover
-- shown while the pointer is on the widget (@""@ = none): the place for the
-- address, the interface, the traffic...
--
-- > networkHover 3 (maybe "offline" (const "wifi")) $ \case
-- >   Nothing -> ""
-- >   Just n -> "<b>" ++ maybe (netInterface n) escape (netName n) ++ "</b>\n"
-- >     ++ maybe "" (++ "\n") (netAddress n)
-- >     ++ "↓ " ++ formatRate (netRxRate n) ++ "  ↑ " ++ formatRate (netTxRate n)
networkHover :: Double -> (Maybe NetInfo -> String) -> (Maybe NetInfo -> String) -> Widget
networkHover secs fmt hov = periodicHoverIO secs $ do
  prev <- newIORef Nothing
  pure ((\n -> (fmt n, hov n)) <$> networkInfo prev)

-- | Sample the network now. The IORef keeps the previous byte counters to
-- compute rates.
networkInfo :: IORef (Maybe (String, Integer, Integer, UTCTime)) -> IO (Maybe NetInfo)
networkInfo prev = do
  miface <- defaultInterface
  case miface of
    Nothing -> writeIORef prev Nothing >> pure Nothing
    Just (iface, gw) -> do
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
        addr6 <- ipv6 iface
        nm <- nmInfo iface
        sig <- case nmSignal =<< nm of
          Just s -> pure (Just s)
          _ | wireless -> procWireless iface
            | otherwise -> pure Nothing
        pure $ Just NetInfo
          { netInterface = iface
          , netKind = if wireless then Wifi else if typ == "1" then Ethernet else OtherNet
          , netName = nmName =<< nm
          , netSignal = sig
          , netAddress = addr
          , netAddress6 = addr6
          , netGateway = gw
          , netFrequency = nmFrequency =<< nm
          , netBitrate = nmBitrate =<< nm
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

-- | The interface of the default route with the lowest metric and its
-- gateway: the IPv4 route, or the IPv6 one on IPv6-only networks.
defaultInterface :: IO (Maybe (String, Maybe String))
defaultInterface = do
  s <- readOr "" "/proc/net/route"
  s6 <- readOr "" "/proc/net/ipv6_route"
  let routes = [ (metric, (iface, dotted gw))
               | (iface : dest : gw : flags : _ : _ : m : _) <- map words (drop 1 (lines s))
               , dest == "00000000"
               , Just f <- [readHex' flags], odd f   -- RTF_UP
               , Just metric <- [readMaybe' m :: Maybe Int] ]
      routes6 = [ (metric, (iface, colons hop))
                | [dest, plen, _, _, hop, m, _, _, flags, iface] <- map words (lines s6)
                , all (== '0') dest, plen == "00", iface /= "lo"
                , Just f <- [readHex' flags], odd f
                , Just metric <- [readHex' m] ]
  pure (snd <$> listToMaybe (sortOn fst routes ++ sortOn fst routes6))
  where
    readHex' x = case reads ("0x" ++ x) :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing
    -- "0101A8C0" (network byte order, read on a little-endian machine)
    dotted x
      | length x == 8, all isHexDigit x, x /= "00000000" =
          Just (intercalate "." (reverse [ show (16 * digitToInt a + digitToInt b) | [a, b] <- chunks 2 x ]))
      | otherwise = Nothing
    -- 32 hex digits; shown in full groups, without the "::" shorthand
    colons x
      | length x == 32, all isHexDigit x, any (/= '0') x =
          Just (intercalate ":" [ let g' = dropWhile (== '0') g in if null g' then "0" else g' | g <- chunks 4 x ])
      | otherwise = Nothing
    chunks n xs = if null xs then [] else take n xs : chunks n (drop n xs)

ipv4 :: String -> IO (Maybe String)
ipv4 iface = withCString iface $ \ci -> allocaBytes 64 $ \out -> do
  ok <- c_ipv4 ci out 64
  if ok == 0 then pure Nothing else Just <$> peekCString out

ipv6 :: String -> IO (Maybe String)
ipv6 iface = withCString iface $ \ci -> allocaBytes 64 $ \out -> do
  ok <- c_ipv6 ci out 64
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

-- | What NetworkManager knows about a device.
data NMInfo = NMInfo
  { nmName      :: Maybe String  -- ^ the SSID for Wi-Fi, else the connection name
  , nmSignal    :: Maybe Int     -- ^ percent
  , nmFrequency :: Maybe Int     -- ^ MHz
  , nmBitrate   :: Maybe Int     -- ^ Mbit/s
  }

nmInfo :: String -> IO (Maybe NMInfo)
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
        case ap of
          Just (VObjectPath a) | a /= "/" -> do
            s <- prop a (nm ++ ".AccessPoint") "Ssid"
            st <- prop a (nm ++ ".AccessPoint") "Strength"
            fr <- prop a (nm ++ ".AccessPoint") "Frequency"
            br <- prop d (nm ++ ".Device.Wireless") "Bitrate"   -- kbit/s
            let ssid = case s of
                  Just (VBytes bs) | not (B.null bs) -> Just (T.unpack (TE.decodeUtf8Lenient bs))
                  _ -> Nothing
            pure (Just (NMInfo (maybe name Just ssid) (st >>= valueInt) (fr >>= valueInt)
                               ((\k -> (k + 500) `div` 1000) <$> (br >>= valueInt))))
          _ -> pure (Just (NMInfo name Nothing Nothing Nothing))
      _ -> pure Nothing

------------------------------------------------------------------------------
-- The details panel and the Wi-Fi menu

-- | Markup for the popover of 'networkHover': the network and its signal,
-- the band and link speed, the addresses, the router and the traffic.
--
-- > networkHover 3 label (maybe "" networkDetails)
networkDetails :: NetInfo -> String
networkDetails n = "<span line_height='1.3'>"
  ++ "<span size='125%'><b>" ++ escapeMarkup (maybe (netInterface n) id (netName n)) ++ "</b></span>\n"
  ++ dim (intercalate " · " (kind : [ x | Just x <- [band, signal] ])) ++ "\n"
  ++ "<span size='40%'> </span>\n"
  ++ concat [ dim l ++ "\t" ++ tnum (escapeMarkup v) ++ "\n" | (l, Just v) <- rows ]
  ++ dim "Traffic" ++ "\t" ++ tnum ("↓ " ++ formatRate (netRxRate n) ++ "   ↑ " ++ formatRate (netTxRate n))
  ++ "</span>"
  where
    kind = case netKind n of
      Wifi -> "Wi-Fi"
      Ethernet -> "Ethernet"
      OtherNet -> "Network"
    band = (\f -> (if f >= 5925 then "6 GHz" else if f >= 4900 then "5 GHz" else "2.4 GHz")
                  ++ maybe "" (\c -> ", channel " ++ show c) (channel f)) <$> netFrequency n
    signal = (\s -> "signal " ++ show s ++ "%") <$> netSignal n
    rows = [ ("IP address", netAddress n)
           , ("IPv6", netAddress6 n)
           , ("Router", netGateway n)
           , ("Interface", Just (netInterface n))
           , ("Link speed", (\r -> show r ++ " Mbit/s") <$> netBitrate n) ]
    dim x = "<span alpha='60%'>" ++ x ++ "</span>"
    tnum x = "<span font_features='tnum'>" ++ x ++ "</span>"
    channel :: Int -> Maybe Int
    channel f
      | f == 2484 = Just 14
      | f >= 2412 && f <= 2472 = Just ((f - 2407) `div` 5)
      | f >= 5160 && f <= 5885 = Just ((f - 5000) `div` 5)
      | f >= 5955 && f <= 7115 = Just ((f - 5950) `div` 5)
      | otherwise = Nothing

-- | A left click opens a menu like the Wi-Fi one of macOS: the network in
-- use (checked), the other known networks in range, the rest under "Other
-- Networks", and a switch for the Wi-Fi radio. Choosing a network connects
-- to it. Needs NetworkManager's @nmcli@; a network with a password that
-- NetworkManager does not know yet has to be joined once by other means
-- (@nmcli --ask device wifi connect NAME@).
--
-- > wifiMenu (networkHover 3 label (maybe "" networkDetails))
wifiMenu :: Widget -> Widget
wifiMenu = onClick $ \c -> when (clickButton c == LeftButton) $ do
  entries <- wifiEntries
  clickMenu c entries
  -- look for networks now, so the next menu is up to date
  void $ forkIO $ void $ nmcli ["device", "wifi", "rescan"]

wifiEntries :: IO [MenuEntry]
wifiEntries = do
  radio <- nmcli ["-t", "-f", "WIFI", "radio"]
  if take 1 (lines radio) /= ["enabled"]
    then pure [ note "Wi-Fi is off", MenuSeparator, menuItem "Turn Wi-Fi On" (run ["radio", "wifi", "on"]) ]
    else do
      aps <- parseAps <$> nmcli ["-t", "-f", "IN-USE,SIGNAL,SECURITY,SSID", "device", "wifi", "list", "--rescan", "no"]
      conns <- nmcli ["-t", "-f", "TYPE,NAME", "connection", "show"]
      let known = [ unescape (drop 1 name) | l <- lines conns, let (t, name) = break (== ':') l, t == "802-11-wireless" ]
          isKnown (_, _, _, ssid) = ssid `elem` known
          current = [ MenuItem (label a) True (Just True) Nothing (pure ()) | a@(True, _, _, _) <- aps ]
          others = [ a | a@(False, _, _, _) <- aps ]
          mine = [ MenuItem (label a) True (Just False) Nothing (run ["connection", "up", "id", ssid])
                 | a@(_, _, _, ssid) <- others, isKnown a ]
          rest = [ menuItem (label a) (run ["device", "wifi", "connect", ssid])
                 | a@(_, _, _, ssid) <- others, not (isKnown a) ]
      pure $ (if null current && null mine then [note "Not connected"] else current ++ mine)
        ++ [ Submenu "Other Networks" rest | not (null rest) ]
        ++ [ MenuSeparator, menuItem "Turn Wi-Fi Off" (run ["radio", "wifi", "off"]) ]
  where
    note l = MenuItem l False Nothing Nothing (pure ())
    run args = void (nmcli args)
    label (_, sig, secure, ssid) = ssid ++ "   " ++ bars sig ++ (if secure then "" else "   open")
    bars sig = let k = max 1 (min 4 ((sig + 24) `div` 25)) in replicate k '●' ++ replicate (4 - k) '○'

-- | The lines of @nmcli -t -f IN-USE,SIGNAL,SECURITY,SSID@: one entry per
-- network name (the strongest access point), the strongest first.
parseAps :: String -> [(Bool, Int, Bool, String)]
parseAps out =
  [ best | ssid <- nub [ s | (_, _, _, s) <- aps ]
         , best <- take 1 (sortOn (\(u, sig, _, _) -> (not u, negate sig)) [ a | a@(_, _, _, s) <- aps, s == ssid ]) ]
  where
    aps = sortOn (\(u, sig, _, _) -> (not u, negate sig))
      [ (use == "*", sig, not (null sec) && sec /= "--", unescape ssid)
      | l <- lines out
      , let (use, r1) = field l; (sigS, r2) = field r1; (sec, ssid) = field r2
      , Just sig <- [readMaybe' sigS], not (null ssid) ]
    field x = let (a, b) = break (== ':') x in (a, drop 1 b)

-- | nmcli's terse output escapes @:@ and @\@ with a backslash.
unescape :: String -> String
unescape ('\\' : c : r) = c : unescape r
unescape (c : r) = c : unescape r
unescape [] = []

-- | The output of nmcli ("" if it is missing or fails).
nmcli :: [String] -> IO String
nmcli args = do
  r <- E.try @E.SomeException (readProcessWithExitCode "nmcli" args "")
  pure $ case r of
    Right (ExitSuccess, out, _) -> out
    _ -> ""

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
