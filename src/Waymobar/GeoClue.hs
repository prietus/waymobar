-- | The position from <https://gitlab.freedesktop.org/geoclue/geoclue GeoClue>,
-- the location service of the system bus. GeoClue combines what it has (Wi-Fi
-- networks in range, a GPS or modem, the IP address) and asks the service set
-- in @\/etc\/geoclue\/geoclue.conf@.
--
-- GeoClue only answers apps it trusts: either an agent is running (the one of
-- GNOME or KDE, or @\/usr\/lib\/geoclue-2.0\/demos\/agent@ elsewhere), or the
-- app's desktop id has its own section in @geoclue.conf@:
--
-- > [waymobar]
-- > allowed=true
-- > system=true
-- > users=
module Waymobar.GeoClue
  ( Fix (..)
  , geoclueFix
  , geoclueWatch
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (void)
import Data.Word (Word32)
import GHC.Clock (getMonotonicTime)
import System.Timeout (timeout)

import Waymobar.DBus

-- | A position.
data Fix = Fix
  { fixLat         :: Double
  , fixLon         :: Double
  , fixAccuracy    :: Double   -- ^ metres
  , fixDescription :: String   -- ^ where it came from, e.g. @"GeoIP (gmaps)"@ or @"WiFi"@
  } deriving (Eq, Show)

gc :: String
gc = "org.freedesktop.GeoClue2"

-- | Ask GeoClue once for the position, as the app @desktopId@ (e.g.
-- @"waymobar"@), waiting at most @secs@ seconds for one within 100 m (the
-- best one seen is returned when the time is up); 'Nothing' if GeoClue is
-- missing, refuses the app, or finds nothing in time.
geoclueFix :: String -> Double -> IO (Maybe Fix)
geoclueFix desktopId secs = geoclueWatch desktopId secs (const (pure ()))

-- | 'geoclueFix', calling @onFix@ with the first position at once and again
-- with every better one as it comes (the quick one from the IP address,
-- then Wi-Fi...), for a caller that can use a rough position meanwhile.
geoclueWatch :: String -> Double -> (Fix -> IO ()) -> IO (Maybe Fix)
geoclueWatch desktopId secs onFix =
  either (const Nothing) id <$> E.try @E.SomeException
    (E.bracket connectSystem disconnect (session desktopId secs onFix))

session :: String -> Double -> (Fix -> IO ()) -> Bus -> IO (Maybe Fix)
session desktopId secs onFix bus = do
  r <- call bus gc "/org/freedesktop/GeoClue2/Manager" (gc ++ ".Manager") "GetClient" []
  case r of
    Right [VObjectPath client] -> do
      let set name v = call bus gc client "org.freedesktop.DBus.Properties" "Set"
            [VString (gc ++ ".Client"), VString name, VVariant v]
      void $ set "DesktopId" (VString desktopId)
      void $ set "RequestedAccuracyLevel" (VUInt32 exact)
      -- LocationUpdated (old, new) carries the path of the new position
      updates <- newChan
      setHandler bus $ \m -> case (msgMember m, msgBody m) of
        (Just "LocationUpdated", [_, VObjectPath new]) | msgPath m == Just client ->
          writeChan updates new
        _ -> pure ()
      addMatch bus ("type='signal',sender='" ++ gc ++ "',interface='" ++ gc
                    ++ ".Client',member='LocationUpdated',path='" ++ client ++ "'")
      started <- call bus gc client (gc ++ ".Client") "Start" []
      case started of
        Left _ -> pure Nothing
        Right _ -> do
          deadline <- (+ secs) <$> getMonotonicTime
          let next = do
                left <- (deadline -) <$> getMonotonicTime
                if left <= 0 then pure Nothing else
                  maybe (pure Nothing) (readFix bus) =<< timeout (micro left) (readChan updates)
              -- the first answer is usually the quick one from the IP
              -- address; Wi-Fi or GPS come some seconds later: keep the
              -- best until one is good enough or the time is up
              refine best
                | fixAccuracy best <= goodEnough = pure best
                | otherwise = next >>= \case
                    Just f | fixAccuracy f < fixAccuracy best -> onFix f >> refine f
                    Just _ -> refine best
                    Nothing -> pure best
          fix <- next >>= traverse (\f -> onFix f >> refine f)
          void $ call bus gc client (gc ++ ".Client") "Stop" []
          pure fix
    _ -> pure Nothing
  where
    exact = 8 :: Word32   -- GCLUE_ACCURACY_LEVEL_EXACT: let every source take part
    goodEnough = 100      -- metres: stop waiting
    micro s = round (s * 1000000 :: Double)

readFix :: Bus -> String -> IO (Maybe Fix)
readFix bus path = do
  r <- call bus gc path "org.freedesktop.DBus.Properties" "GetAll" [VString (gc ++ ".Location")]
  pure $ case r of
    Right [props] -> do
      lat <- double =<< dictLookup "Latitude" props
      lon <- double =<< dictLookup "Longitude" props
      let acc = maybe 0 id (double =<< dictLookup "Accuracy" props)
          desc = maybe "" id (valueString =<< dictLookup "Description" props)
      pure (Fix lat lon acc desc)
    _ -> Nothing
  where
    double v = case fromVariant v of
      VDouble d -> Just d
      _ -> Nothing
