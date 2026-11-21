-- | The power profile of power-profiles-daemon (or tuned-ppd): power
-- saver, balanced or performance, over D-Bus on the system bus. It
-- follows changes made elsewhere (GNOME's settings, @powerprofilesctl@)
-- at once.
module Waymobar.Power
  ( PowerProfile (..)
  , PowerState (..)
  , powerProfile
  , powerProfileHover
  , profileName
  ) where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forM_, forever, unless, void)
import Data.IORef
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO (hPutStrLn, stderr)

import Waymobar.Core
import Waymobar.DBus

data PowerProfile = PowerSaver | Balanced | Performance
  deriving (Eq, Ord, Show, Enum, Bounded)

data PowerState = PowerState
  { powerActive    :: PowerProfile
  , powerAvailable :: [PowerProfile]  -- ^ those this machine has (performance needs a driver for it)
  , powerDegraded  :: String          -- ^ why performance is held back (@"lap-detected"@, @"high-operating-temperature"@...); @""@ when it is not
  } deriving (Eq, Show)

-- | The name in the daemon's terms: @"power-saver"@, @"balanced"@,
-- @"performance"@.
profileId :: PowerProfile -> String
profileId = \case
  PowerSaver -> "power-saver"
  Balanced -> "balanced"
  Performance -> "performance"

-- | For people: @"Power Saver"@, @"Balanced"@, @"Performance"@.
profileName :: PowerProfile -> String
profileName = \case
  PowerSaver -> "Power Saver"
  Balanced -> "Balanced"
  Performance -> "Performance"

parseProfile :: String -> Maybe PowerProfile
parseProfile s = lookup s [ (profileId p, p) | p <- [minBound .. maxBound] ]

-- The current name of the service, and the one of older daemons.
services :: [(String, String, String)]
services =
  [ ("org.freedesktop.UPower.PowerProfiles", "/org/freedesktop/UPower/PowerProfiles", "org.freedesktop.UPower.PowerProfiles")
  , ("net.hadess.PowerProfiles", "/net/hadess/PowerProfiles", "net.hadess.PowerProfiles") ]

-- | The power profile: @f@ gets 'Nothing' when there is no daemon. A left
-- click moves to the next profile, the wheel goes up and down them, and a
-- right click opens a menu to pick one. Return @""@ to hide the widget.
--
-- > powerProfile $ \case
-- >   Nothing -> ""
-- >   Just p -> profileName (powerActive p)
powerProfile :: (Maybe PowerState -> String) -> Widget
powerProfile fmt = powerProfileHover fmt (const "")

-- | Like 'powerProfile', with the markup of a popover shown while the
-- pointer is on it (@""@ = none).
--
-- > powerProfileHover label (\p -> "<b>" ++ profileName (powerActive p) ++ "</b>\nclick for the next one")
powerProfileHover :: (Maybe PowerState -> String) -> (PowerState -> String) -> Widget
powerProfileHover fmt hov = Widget $ \env -> do
  ref <- newIORef Nothing
  busRef <- newIORef Nothing
  let refresh bus = do
        new <- readState bus
        old <- atomicModifyIORef' ref (\o -> (new, o))
        unless (new == old) (redraw env)
  void $ forkIO $ do
    r <- E.try $ do
      bus <- connectSystem
      writeIORef busRef (Just bus)
      setHandler bus $ \m -> case (msgInterface m, msgMember m, msgBody m) of
        -- a property changed, or the daemon came or went
        (Just "org.freedesktop.DBus.Properties", Just "PropertiesChanged", VString iface : _)
          | iface `elem` [ i | (_, _, i) <- services ] -> void (forkIO (refresh bus))
        (Just "org.freedesktop.DBus", Just "NameOwnerChanged", VString name : _)
          | name `elem` [ n | (n, _, _) <- services ] -> void (forkIO (refresh bus))
        _ -> pure ()
      forM_ services $ \(_, path, _) ->
        addMatch bus ("type='signal',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged',path='" ++ path ++ "'")
      addMatch bus "type='signal',sender='org.freedesktop.DBus',interface='org.freedesktop.DBus',member='NameOwnerChanged'"
      refresh bus
      -- in case a signal was missed
      forever (threadDelay 60000000 >> refresh bus)
    case r of
      Left e -> hPutStrLn stderr ("waymobar: power profiles: " ++ show (e :: E.SomeException))
      Right () -> pure ()
  let set p = readIORef busRef >>= mapM_ (\bus -> setPowerProfile bus p >> refresh bus)
      click st c = case clickButton c of
        LeftButton -> set (step 1 st)
        ScrollUp -> set (step 1 st)
        ScrollDown -> set (step (-1) st)
        RightButton -> clickMenu c
          [ MenuItem (profileName p) True (Just (p == powerActive st)) Nothing (set p) | p <- powerAvailable st ]
        _ -> pure ()
  pure $ \_ -> do
    ms <- readIORef ref
    let s = fmt ms
        h = maybe "" hov ms
    pure [ (block s) { blockOnClick = click <$> ms, blockHoverMarkup = if null h then Nothing else Just h }
         | not (null s) ]
  where
    -- the next (or previous) available profile, round and round
    step n st =
      let ps = powerAvailable st
          i = fromMaybe 0 (elemIndex (powerActive st) ps)
      in if null ps then powerActive st else ps !! ((i + n) `mod` length ps)

-- | Ask the daemon for the profile and what it offers.
readState :: Bus -> IO (Maybe PowerState)
readState bus = go services
  where
    go [] = pure Nothing
    go ((name, path, iface) : rest) = do
      r <- call bus name path "org.freedesktop.DBus.Properties" "GetAll" [VString iface]
      case r of
        Right [props] | Just active <- dictLookup "ActiveProfile" props >>= valueString >>= parseProfile ->
          pure $ Just PowerState
            { powerActive = active
            , powerAvailable = case dictLookup "Profiles" props of
                Just (VArray _ ps) -> mapMaybe (\p -> dictLookup "Profile" p >>= valueString >>= parseProfile) ps
                _ -> [active]
            , powerDegraded = fromMaybe "" (dictLookup "PerformanceDegraded" props >>= valueString) }
        _ -> go rest

-- | Switch profile (the daemon may refuse, if polkit does not allow it).
setPowerProfile :: Bus -> PowerProfile -> IO ()
setPowerProfile bus p = go services Nothing
  where
    go [] firstErr = mapM_ (\e -> hPutStrLn stderr ("waymobar: power profiles: " ++ e)) firstErr
    go ((name, path, iface) : rest) firstErr = do
      r <- call bus name path "org.freedesktop.DBus.Properties" "Set"
             [VString iface, VString "ActiveProfile", VVariant (VString (profileId p))]
      case r of
        Right _ -> pure ()
        Left err -> go rest (Just (fromMaybe err firstErr))
