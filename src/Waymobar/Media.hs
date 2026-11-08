-- | Media players through MPRIS (Spotify, Firefox, Chromium, mpv with
-- mpv-mpris, VLC, ...) on the session bus. Updates as players change;
-- shows the playing player, or else the one that changed last.
module Waymobar.Media
  ( Media (..)
  , PlaybackStatus (..)
  , mpris
  , MediaControl (..)
  , mediaControl
  ) where

import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import System.IO (hPutStrLn, stderr)

import Waymobar.Core
import Waymobar.DBus

data PlaybackStatus = Playing | Paused | Stopped
  deriving (Eq, Show)

data Media = Media
  { mediaPlayer :: String          -- ^ e.g. "spotify", "firefox"
  , mediaStatus :: PlaybackStatus
  , mediaTitle  :: String
  , mediaArtist :: String          -- ^ the artists, joined with ", "
  , mediaAlbum  :: String
  } deriving (Eq, Show)

data Player = Player
  { plName  :: String   -- ^ org.mpris.MediaPlayer2.foo
  , plOwner :: String   -- ^ unique name, to match signals
  , plMedia :: Media
  , plStamp :: Int      -- ^ when it last changed, to pick a player
  }

-- | What the default clicks do on the shown player.
data MediaControl = PlayPause | Next | Previous | Stop
  deriving (Eq, Show)

prefix, playerPath, playerIface :: String
prefix = "org.mpris.MediaPlayer2."
playerPath = "/org/mpris/MediaPlayer2"
playerIface = "org.mpris.MediaPlayer2.Player"

-- | The current media player, formatted by the function (which should
-- 'Waymobar.Widgets.escape' titles). Hidden without players, or when the
-- function returns "". Left click: play/pause; right click: next; middle
-- click: previous.
--
-- > mpris (\m -> (if mediaStatus m == Playing then "▶ " else "⏸ ")
-- >             ++ escape (shorten 40 (mediaArtist m ++ " - " ++ mediaTitle m)))
mpris :: (Media -> String) -> Widget
mpris fmt = Widget $ \env -> do
  players <- newIORef (M.empty :: M.Map String Player)
  stamp <- newIORef (0 :: Int)
  busRef <- newIORef Nothing
  void $ forkIO $ do
    r <- E.try (start env players stamp)
    case r of
      Left e -> hPutStrLn stderr ("waymobar: mpris disabled: " ++ show (e :: E.SomeException))
      Right bus -> writeIORef busRef (Just bus)
  pure $ \_ -> do
    ps <- readIORef players
    mb <- readIORef busRef
    pure $ case (pick (M.elems ps), mb) of
      (Just p, Just bus) ->
        let m = fmt (plMedia p)
        in [ (block m) { blockOnClick = Just (click bus p) } | not (null m) ]
      _ -> []
  where
    click bus p c = case clickButton c of
      LeftButton -> mediaControl' bus p PlayPause
      RightButton -> mediaControl' bus p Next
      MiddleButton -> mediaControl' bus p Previous
      _ -> pure ()

-- | Playing players first, then the most recently changed.
pick :: [Player] -> Maybe Player
pick = listToMaybe . sortOn (\p -> (mediaStatus (plMedia p) /= Playing, Down (plStamp p)))

mediaControl' :: Bus -> Player -> MediaControl -> IO ()
mediaControl' bus p ctl = void $ call bus (plName p) playerPath playerIface (show ctl) []

-- | Send a command to the player that 'mpris' would show (for key
-- bindings, run @waymobar@'s own widget clicks instead when possible).
mediaControl :: MediaControl -> IO ()
mediaControl ctl = do
  bus <- connectSession
  names <- playerNames bus
  ps <- mapMaybe id <$> mapM (\(i, n) -> fmap (\m -> Player n n m i) <$> fetch bus n) (zip [0 ..] names)
  forM_ (pick ps) $ \p -> mediaControl' bus p ctl

start :: BarEnv -> IORef (M.Map String Player) -> IORef Int -> IO Bus
start env players stamp = do
  bus <- connectSession
  let refreshName name owner = do
        mm <- fetch bus name
        n <- atomicModifyIORef' stamp (\x -> (x + 1, x + 1))
        atomicModifyIORef' players $ \ps -> case mm of
          Nothing -> (M.delete name ps, ())
          Just m -> (M.insert name (Player name owner m n) ps, ())
        redraw env
      ownerOf name = do
        r <- call bus "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "GetNameOwner" [VString name]
        pure $ case r of
          Right [VString o] -> o
          _ -> name
  setHandler bus $ \m -> case (msgType m, msgInterface m, msgMember m, msgBody m) of
    (Signal, Just "org.freedesktop.DBus", Just "NameOwnerChanged", [VString name, VString _, VString new])
      | prefix `isPrefixOf` name ->
          if null new
            then atomicModifyIORef' players (\ps -> (M.delete name ps, ())) >> redraw env
            else refreshName name new
    (Signal, Just "org.freedesktop.DBus.Properties", Just "PropertiesChanged", VString i : _)
      | i == playerIface, Just sender <- msgSender m -> do
          ps <- readIORef players
          forM_ [ p | p <- M.elems ps, plOwner p == sender ] $ \p -> refreshName (plName p) sender
    _ -> pure ()
  addMatch bus ("type='signal',sender='org.freedesktop.DBus',member='NameOwnerChanged',arg0namespace='org.mpris.MediaPlayer2'")
  addMatch bus ("type='signal',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged',path='" ++ playerPath ++ "'")
  names <- playerNames bus
  forM_ names $ \name -> ownerOf name >>= refreshName name
  pure bus

playerNames :: Bus -> IO [String]
playerNames bus = do
  r <- call bus "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames" []
  pure $ case r of
    Right [VArray _ ns] -> [ n | VString n <- ns, prefix `isPrefixOf` n ]
    _ -> []

-- | A player's state; Nothing if it does not answer.
fetch :: Bus -> String -> IO (Maybe Media)
fetch bus name = do
  r <- call bus name playerPath "org.freedesktop.DBus.Properties" "GetAll" [VString playerIface]
  pure $ case r of
    Right [d] ->
      let meta = fromMaybe (VArray TVariant []) (dictLookup "Metadata" d)
          str k = fromMaybe "" (dictLookup k meta >>= valueString)
          artists = case dictLookup "xesam:artist" meta of
            Just (VArray _ as) -> [ a | VString a <- map fromVariant as ]
            Just v | Just a <- valueString v -> [a]
            _ -> []
          status = case dictLookup "PlaybackStatus" d >>= valueString of
            Just "Playing" -> Playing
            Just "Paused" -> Paused
            _ -> Stopped
      in Just Media
           { mediaPlayer = takeWhile (/= '.') (drop (length prefix) name)
           , mediaStatus = status
           , mediaTitle = str "xesam:title"
           , mediaArtist = joinWith ", " artists
           , mediaAlbum = str "xesam:album"
           }
    _ -> Nothing
  where
    joinWith sep = foldr1' (\a b -> a ++ sep ++ b)
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs
