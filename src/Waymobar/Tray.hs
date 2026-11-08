-- | A system tray: the StatusNotifierItem protocol over D-Bus (what
-- KDE, libappindicator, Electron, Qt, nm-applet, ... use on Wayland).
--
-- waymobar becomes the @org.kde.StatusNotifierWatcher@ if nobody else is,
-- and registers itself as a host either way. Item menus
-- (@com.canonical.dbusmenu@) are shown as waymobar popups.
module Waymobar.Tray
  ( TrayConfig (..)
  , defaultTray
  , tray
  ) where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import System.Environment (lookupEnv)
import Data.Ord (Down (..))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)

import Waymobar.Core
import Waymobar.Draw (Color (..), parseColor)
import Waymobar.DBus
import Waymobar.Icons

data TrayConfig = TrayConfig
  { trayIconSize   :: Int       -- ^ logical pixels; 0 = the bar height minus 8
  , trayPadding    :: Int       -- ^ pixels at each side of an icon
  , trayIconThemes :: [String]  -- ^ icon themes to search; [] = the GTK theme, Adwaita, hicolor
  , trayShowPassive :: Bool     -- ^ also show items that say they are idle
  , traySymbolicColor :: String -- ^ colour for monochrome @-symbolic@ icons
  }

defaultTray :: TrayConfig
defaultTray = TrayConfig 0 3 [] False "#d8dee9"

data Item = Item
  { itService :: String   -- ^ bus name to call
  , itPath    :: String
  , itOwner   :: String   -- ^ unique bus name, for signals
  , itOrder   :: Int
  , itStatus  :: String
  , itIcon    :: Maybe Icon
  , itIconSource :: Maybe IconSource  -- ^ theme icon, rasterized at the real size when drawn
  , itTitle   :: String
  , itMenu    :: Maybe String
  , itIsMenu  :: Bool
  }

data TrayState = TrayState
  { tsItems   :: IORef (M.Map String Item)   -- ^ by "service/path"
  , tsNext    :: IORef Int
  , tsWatcher :: IORef Bool                  -- ^ we are the watcher
  , tsThemes  :: [String]
  , tsRedraw  :: IO ()
  , tsSymbolic :: (Double, Double, Double)
  }

watcherName, watcherPath, watcherIface, itemIface, menuIface :: String
watcherName = "org.kde.StatusNotifierWatcher"
watcherPath = "/StatusNotifierWatcher"
watcherIface = "org.kde.StatusNotifierWatcher"
itemIface = "org.kde.StatusNotifierItem"
menuIface = "com.canonical.dbusmenu"

-- | The tray. Use it once (it registers on the session bus).
tray :: TrayConfig -> Widget
tray cfg = Widget $ \env -> do
  itemsRef <- newIORef M.empty
  busRef <- newIORef Nothing
  void $ forkIO $ do
    r <- E.try $ do
      bus <- connectSession
      themes <- if null (trayIconThemes cfg) then defaultIconThemes else pure (trayIconThemes cfg)
      let symbolic = case parseColor (traySymbolicColor cfg) of
            Just (Color r g b _) -> (r, g, b)
            Nothing -> (0.85, 0.87, 0.91)
      st <- TrayState itemsRef <$> newIORef 0 <*> newIORef False <*> pure themes <*> pure (redraw env) <*> pure symbolic
      writeIORef busRef (Just (bus, st))
      start bus st
    case r of
      Left e -> hPutStrLn stderr ("waymobar: tray disabled: " ++ show (e :: E.SomeException))
      Right () -> pure ()
  pure $ \_ -> do
    items <- readIORef itemsRef
    mb <- readIORef busRef
    pure
      [ (block "")
          { blockIcon = Just icon
          , blockIconSource = itIconSource it
          , blockIconColor = Just (traySymbolicColor cfg)
          , blockIconSize = trayIconSize cfg
          , blockPadding = trayPadding cfg
          , blockOnClick = (\(bus, st) -> clickItem bus st it) <$> mb }
      | it <- sortOn itOrder (M.elems items)
      , trayShowPassive cfg || itStatus it /= "Passive"
      , Just icon <- [itIcon it] ]

------------------------------------------------------------------------------
-- Watcher and host

-- | Trace tray activity on stderr when WAYMOBAR_DEBUG is set.
debug :: String -> IO ()
debug msg = do
  on <- isJust <$> lookupEnv "WAYMOBAR_DEBUG"
  when on $ hPutStrLn stderr ("waymobar tray: " ++ msg)

start :: Bus -> TrayState -> IO ()
start bus st = do
  setHandler bus (handleMessage bus st)
  addMatch bus ("type='signal',interface='" ++ itemIface ++ "'")
  addMatch bus "type='signal',sender='org.freedesktop.DBus',interface='org.freedesktop.DBus',member='NameOwnerChanged'"
  addMatch bus ("type='signal',interface='" ++ watcherIface ++ "'")
  becomeWatcherOrHost bus st

becomeWatcherOrHost :: Bus -> TrayState -> IO ()
becomeWatcherOrHost bus st = do
  isWatcher <- requestName bus watcherName 4  -- DBUS_NAME_FLAG_DO_NOT_QUEUE
  writeIORef (tsWatcher st) isWatcher
  debug (if isWatcher then "acting as the watcher" else "another watcher exists; registering as a host")
  if isWatcher
    then emitSignal bus watcherPath watcherIface "StatusNotifierHostRegistered" []
    else do
      pid <- getProcessID
      let host = "org.kde.StatusNotifierHost-" ++ show pid
      _ <- requestName bus host 4
      _ <- call bus watcherName watcherPath watcherIface "RegisterStatusNotifierHost" [VString host]
      r <- call bus watcherName watcherPath "org.freedesktop.DBus.Properties" "Get"
             [VString watcherIface, VString "RegisteredStatusNotifierItems"]
      case r of
        Right [v] | VArray _ ids <- fromVariant v ->
          forM_ ids $ \case
            VString i -> void (forkIO (addItemById bus st i))
            _ -> pure ()
        _ -> pure ()

handleMessage :: Bus -> TrayState -> Message -> IO ()
handleMessage bus st m = case msgType m of
  MethodCall -> do
    w <- readIORef (tsWatcher st)
    if w && msgPath m == Just watcherPath then watcherCall else unknown
  Signal -> case (msgInterface m, msgMember m, msgBody m) of
    (Just "org.freedesktop.DBus", Just "NameOwnerChanged", [VString name, VString _, VString new])
      | null new -> do
          removeWhere (\it -> itOwner it == name || itService it == name)
          w <- readIORef (tsWatcher st)
          when (name == watcherName && not w) $ do
            -- the other watcher went away: take over, keeping the items we
            -- know (applications also register again with the new watcher)
            threadDelay 200000
            becomeWatcherOrHost bus st
    (Just i, Just "StatusNotifierItemRegistered", [VString ident]) | i == watcherIface -> do
      w <- readIORef (tsWatcher st)
      unless w $ void (forkIO (addItemById bus st ident))
    (Just i, Just "StatusNotifierItemUnregistered", [VString ident]) | i == watcherIface -> do
      w <- readIORef (tsWatcher st)
      unless w $ removeWhere (\it -> itService it ++ itPath it == ident)
    (Just i, Just _, _) | i == itemIface -> do
      items <- readIORef (tsItems st)
      forM_ [ it | it <- M.elems items, Just (itOwner it) == msgSender m ] $ \it ->
        void (forkIO (refresh bus st (itService it) (itPath it)))
    _ -> pure ()
  _ -> pure ()
  where
    unknown = replyError bus m "org.freedesktop.DBus.Error.UnknownMethod" "unknown method"
    sender = fromMaybe "" (msgSender m)
    removeWhere p = do
      gone <- atomicModifyIORef' (tsItems st) $ \items ->
        let (out, keep) = M.partition p items in (keep, M.keys out)
      unless (null gone) $ do
        w <- readIORef (tsWatcher st)
        when w $ forM_ gone $ \k -> emitSignal bus watcherPath watcherIface "StatusNotifierItemUnregistered" [VString k]
        tsRedraw st
    watcherCall = case (msgInterface m, msgMember m, msgBody m) of
      (Just i, Just "RegisterStatusNotifierItem", [VString s]) | i == watcherIface -> do
        let (service, path) = if "/" `isPrefixOf` s then (sender, s) else (s, "/StatusNotifierItem")
        reply bus m []
        debug ("register " ++ service ++ path)
        emitSignal bus watcherPath watcherIface "StatusNotifierItemRegistered" [VString (service ++ path)]
        n <- atomicModifyIORef' (tsNext st) (\x -> (x + 1, x))
        void (forkIO (addItem bus st n service path))
      (Just i, Just "RegisterStatusNotifierHost", _) | i == watcherIface -> reply bus m []
      (Just "org.freedesktop.DBus.Properties", Just "Get", [VString _, VString prop]) ->
        case lookup prop props of
          Just v -> v >>= \x -> reply bus m [VVariant x]
          Nothing -> replyError bus m "org.freedesktop.DBus.Error.UnknownProperty" prop
      (Just "org.freedesktop.DBus.Properties", Just "GetAll", _) -> do
        kvs <- forM props $ \(k, v) -> (\x -> VDictEntry (VString k) (VVariant x)) <$> v
        reply bus m [VArray (TDictEntry TString TVariant) kvs]
      (Just "org.freedesktop.DBus.Introspectable", Just "Introspect", _) -> reply bus m [VString introspection]
      (Just "org.freedesktop.DBus.Peer", Just "Ping", _) -> reply bus m []
      _ -> unknown
    props =
      [ ("RegisteredStatusNotifierItems", do
            items <- readIORef (tsItems st)
            pure (VArray TString [ VString (itService it ++ itPath it) | it <- M.elems items ]))
      , ("IsStatusNotifierHostRegistered", pure (VBool True))
      , ("ProtocolVersion", pure (VInt32 0)) ]

introspection :: String
introspection = unlines
  [ "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\""
  , " \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">"
  , "<node>"
  , " <interface name=\"org.kde.StatusNotifierWatcher\">"
  , "  <method name=\"RegisterStatusNotifierItem\"><arg name=\"service\" type=\"s\" direction=\"in\"/></method>"
  , "  <method name=\"RegisterStatusNotifierHost\"><arg name=\"service\" type=\"s\" direction=\"in\"/></method>"
  , "  <property name=\"RegisteredStatusNotifierItems\" type=\"as\" access=\"read\"/>"
  , "  <property name=\"IsStatusNotifierHostRegistered\" type=\"b\" access=\"read\"/>"
  , "  <property name=\"ProtocolVersion\" type=\"i\" access=\"read\"/>"
  , "  <signal name=\"StatusNotifierItemRegistered\"><arg type=\"s\"/></signal>"
  , "  <signal name=\"StatusNotifierItemUnregistered\"><arg type=\"s\"/></signal>"
  , "  <signal name=\"StatusNotifierHostRegistered\"/>"
  , " </interface>"
  , "</node>" ]

-- | "service/path" (or just "service") as stored by watchers.
addItemById :: Bus -> TrayState -> String -> IO ()
addItemById bus st ident = do
  let (service, path) = break (== '/') ident
  n <- atomicModifyIORef' (tsNext st) (\x -> (x + 1, x))
  addItem bus st n service (if null path then "/StatusNotifierItem" else path)

addItem :: Bus -> TrayState -> Int -> String -> String -> IO ()
addItem bus st n service path = do
  owner <- if ":" `isPrefixOf` service
    then pure service
    else do
      r <- call bus "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "GetNameOwner" [VString service]
      pure $ case r of
        Right [VString o] -> o
        _ -> service
  let key = service ++ path
  modifyIORef' (tsItems st) $ M.insertWith (\_ old -> old) key
    (Item service path owner n "Active" Nothing Nothing "" Nothing False)
  refresh bus st service path

-- | A themed icon, recoloured if it is a symbolic one.
themedIcon :: TrayState -> [FilePath] -> String -> Int -> IO (Maybe Icon)
themedIcon st dirs = lookupIcon (Just (tsSymbolic st)) dirs (tsThemes st)

-- | Fetch an item's properties and icon again.
refresh :: Bus -> TrayState -> String -> String -> IO ()
refresh bus st service path = do
  r <- call bus service path "org.freedesktop.DBus.Properties" "GetAll" [VString itemIface]
  let key = service ++ path
  debug ("refresh " ++ key ++ ": " ++ either id (const "ok") r)
  case r of
    Left _ -> do
      -- give a starting application a second chance before dropping it
      threadDelay 500000
      r' <- call bus service path "org.freedesktop.DBus.Properties" "GetAll" [VString itemIface]
      case r' of
        Right [d] -> update key d
        _ -> modifyIORef' (tsItems st) (M.delete key) >> tsRedraw st
    Right [d] -> update key d
    Right _ -> pure ()
  where
    update key d = do
      let str k = fromMaybe "" (dictLookup k d >>= valueString)
          status = str "Status"
          attention = status == "NeedsAttention"
          name = if attention && not (null (str "AttentionIconName")) then str "AttentionIconName" else str "IconName"
          pixKey = if attention && isJust' (dictLookup "AttentionIconPixmap" d) then "AttentionIconPixmap" else "IconPixmap"
          themePath = [ p | let p = str "IconThemePath", not (null p) ]
      fromTheme <- if null name then pure Nothing else themed themePath name 64
      let icon = case fromTheme of
            Just i -> Just i
            Nothing -> dictLookup pixKey d >>= pixmap
          menu = case dictLookup "Menu" d >>= valueString of
            Just p | p /= "/" && not (null p) -> Just p
            _ -> Nothing
      debug ("item " ++ key ++ ": status " ++ show status ++ ", icon " ++ show name ++ " -> " ++ show icon)
      modifyIORef' (tsItems st) $ M.adjust (\it -> it
        { itStatus = if null status then "Active" else status
        , itIcon = icon
        , itIconSource = if isJust fromTheme then Just (IconName themePath name) else Nothing
        , itTitle = str "Title"
        , itMenu = menu
        , itIsMenu = fromMaybe False (dictLookup "ItemIsMenu" d >>= valueBool)
        }) key
      tsRedraw st
    isJust' = maybe False (const True)
    themed = themedIcon st
    -- the largest of the offered sizes
    pixmap v = case fromVariant v of
      VArray _ ps -> listToMaybe $ mapMaybe (\(w, h, bs) -> iconFromNetworkARGB w h bs) $ sortOn (\(w, _, _) -> Down w)
        [ (fromIntegral w, fromIntegral h, bs) | VStruct [VInt32 w, VInt32 h, VBytes bs] <- ps ]
      _ -> Nothing

------------------------------------------------------------------------------
-- Clicks and menus

clickItem :: Bus -> TrayState -> Item -> Click -> IO ()
clickItem bus st it c = case clickButton c of
  LeftButton
    | itIsMenu it, Just _ <- itMenu it -> showMenu
    | otherwise -> do
        r <- call bus (itService it) (itPath it) itemIface "Activate" [VInt32 0, VInt32 0]
        case (r, itMenu it) of
          (Left _, Just _) -> showMenu
          _ -> pure ()
  MiddleButton -> void $ call bus (itService it) (itPath it) itemIface "SecondaryActivate" [VInt32 0, VInt32 0]
  RightButton
    | Just _ <- itMenu it -> showMenu
    | otherwise -> void $ call bus (itService it) (itPath it) itemIface "ContextMenu" [VInt32 0, VInt32 0]
  ScrollUp -> void $ call bus (itService it) (itPath it) itemIface "Scroll" [VInt32 120, VString "vertical"]
  ScrollDown -> void $ call bus (itService it) (itPath it) itemIface "Scroll" [VInt32 (-120), VString "vertical"]
  _ -> pure ()
  where
    showMenu = forM_ (itMenu it) $ \mpath -> do
      entries <- fetchMenu bus st (itService it) mpath
      unless (null entries) (clickMenu c entries)

-- | Read a dbusmenu as menu entries whose actions send "clicked".
fetchMenu :: Bus -> TrayState -> String -> String -> IO [MenuEntry]
fetchMenu bus st service path = do
  _ <- call bus service path menuIface "AboutToShow" [VInt32 0]
  r <- call bus service path menuIface "GetLayout" [VInt32 0, VInt32 (-1), VArray TString []]
  case r of
    Right [_, VStruct [_, _, VArray _ kids]] -> entries kids
    _ -> pure []
  where
    themed = themedIcon st
    entries kids = catMaybesM (map (conv . fromVariant) kids)
    catMaybesM = fmap (mapMaybe id) . sequence
    conv (VStruct [VInt32 i, props, VArray _ kids]) = do
      let str k = dictLookup k props >>= valueString
          bool k d = fromMaybe d (dictLookup k props >>= valueBool)
          visible = bool "visible" True
          label = stripMnemonic (fromMaybe "" (str "label"))
          toggle = case str "toggle-type" of
            Just t | t `elem` ["checkmark", "radio"] -> Just ((dictLookup "toggle-state" props >>= valueInt) == Just 1)
            _ -> Nothing
          isSub = str "children-display" == Just "submenu" || not (null kids)
      if not visible
        then pure Nothing
        else if str "type" == Just "separator"
          then pure (Just MenuSeparator)
          else if isSub
            then do
              sub <- entries kids
              pure (Just (Submenu label sub))
            else do
              icon <- case str "icon-name" of
                Just n | not (null n) -> themed [] n 32
                _ -> pure Nothing
              pure $ Just MenuItem
                { itemLabel = label
                , itemEnabled = bool "enabled" True
                , itemToggle = toggle
                , itemIcon = icon
                , itemAction = void $ call bus service path menuIface "Event"
                    [VInt32 i, VString "clicked", VVariant (VInt32 0), VUInt32 0]
                }
    conv _ = pure Nothing

-- | "_File" -> "File", "a__b" -> "a_b".
stripMnemonic :: String -> String
stripMnemonic = \case
  '_' : '_' : r -> '_' : stripMnemonic r
  '_' : r -> stripMnemonic r
  x : r -> x : stripMnemonic r
  [] -> []
