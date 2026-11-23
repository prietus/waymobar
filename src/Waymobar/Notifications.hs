-- | Notifications from mako: how many arrived since you last looked, a
-- "do not disturb" switch and the recent ones, like a small notification
-- centre. It reads @makoctl@, so it needs mako as the notification daemon.
--
-- > , notifications defaultNotifications
--
-- A click opens a menu with the recent notifications (newest first),
-- "Do Not Disturb", "Show the Last One Again" and "Dismiss All"; the
-- right button switches "do not disturb" at once. With the pointer on it,
-- a popover lists the recent ones.
--
-- "Do not disturb" is mako's mode @do-not-disturb@, which has to hide the
-- notifications in mako's config (@~/.config/mako/config@):
--
-- > [mode=do-not-disturb]
-- > invisible=1
--
-- Notifications that arrive meanwhile still go to the history, so they
-- show up here.
module Waymobar.Notifications
  ( notifications
  , NotificationsConfig (..)
  , defaultNotifications
  , NotifState (..)
  , Notification (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import Data.IORef
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

import Waymobar.Core
import qualified Waymobar.JSON as J
import Waymobar.Widgets (escape, shorten)

data Notification = Notification
  { notifId      :: Int
  , notifApp     :: String
  , notifSummary :: String
  , notifBody    :: String
  , notifUrgent  :: Bool
  } deriving (Eq, Show)

-- | What the label and the popover are drawn from.
data NotifState = NotifState
  { nsDnd     :: Bool            -- ^ "do not disturb" is on
  , nsUnseen  :: Int             -- ^ arrived since the menu was last opened
  , nsShowing :: [Notification]  -- ^ on screen now
  , nsHistory :: [Notification]  -- ^ gone from the screen, newest first
  } deriving (Eq, Show)

data NotificationsConfig = NotificationsConfig
  { notifInterval :: Double                -- ^ seconds between looks at mako
  , notifDndMode  :: String                -- ^ mako's mode for "do not disturb"
  , notifRecent   :: Int                   -- ^ how many to list in the menu and popover
  , notifLabel    :: NotifState -> String  -- ^ markup of the bar
  , notifHover    :: NotifState -> String  -- ^ markup of the popover ("" = none)
  }

-- | A bell (crossed out in "do not disturb") and the number of new ones;
-- the popover lists the recent ones.
defaultNotifications :: NotificationsConfig
defaultNotifications = NotificationsConfig
  { notifInterval = 2
  , notifDndMode = "do-not-disturb"
  , notifRecent = 8
  , notifLabel = \s -> (if nsDnd s then "🔕" else "🔔")
      ++ (if nsUnseen s > 0 then " " ++ show (nsUnseen s) else "")
  , notifHover = recentMarkup 8
  }

-- | The recent notifications, as popover markup.
recentMarkup :: Int -> NotifState -> String
recentMarkup n s = case take n (nsShowing s ++ nsHistory s) of
  [] -> if nsDnd s then "Do not disturb · no notifications" else "No notifications"
  ns -> (if nsDnd s then "<i>Do not disturb</i>\n" else "")
    ++ concatMap one ns
  where
    one x = "<b>" ++ escape (shorten 50 (notifSummary x)) ++ "</b>"
      ++ "  <small>" ++ escape (notifApp x) ++ "</small>\n"
      ++ (if null (notifBody x) then "" else escape (shorten 70 (firstLine (notifBody x))) ++ "\n")
    firstLine = takeWhile (/= '\n')

notifications :: NotificationsConfig -> Widget
notifications cfg = Widget $ \env -> do
  ref <- newIORef (NotifState False 0 [] [])
  seen <- newIORef (Nothing :: Maybe Int)   -- the newest id when the menu was last opened
  let refresh = do
        dnd <- (notifDndMode cfg `elem`) . lines <$> makoctl ["mode"]
        showing <- parseList <$> makoctl ["list", "-j"]
        hist <- parseList <$> makoctl ["history", "-j"]
        let newest = maximum (0 : map notifId (showing ++ hist))
        mark <- readIORef seen
        -- the first look sets the mark: nothing is "new" at start-up
        lastSeen <- case mark of
          Nothing -> writeIORef seen (Just newest) >> pure newest
          Just m -> pure m
        let unseen = length [ x | x <- showing ++ hist, notifId x > lastSeen ]
            st = NotifState dnd unseen showing hist
        old <- atomicModifyIORef' ref (\o -> (st, o))
        when (st /= old) (redraw env)
      safely act = void (E.try act :: IO (Either E.SomeException ()))
  void $ forkIO $ forever $ safely refresh >> threadDelay (round (notifInterval cfg * 1000000))
  let markSeen = do
        st <- readIORef ref
        writeIORef seen (Just (maximum (0 : map notifId (nsShowing st ++ nsHistory st))))
        safely refresh
      toggleDnd = void (makoctl ["mode", "-t", notifDndMode cfg]) >> safely refresh
      click c = case clickButton c of
        RightButton -> toggleDnd
        LeftButton -> do
          st <- readIORef ref
          markSeen
          let recent = take (notifRecent cfg) (nsShowing st ++ nsHistory st)
              entry x = MenuItem (shorten 60 (notifSummary x ++ (if null (notifApp x) then "" else "  ·  " ++ notifApp x)))
                                 False Nothing Nothing (pure ())
          clickMenu c $
            (if null recent then [MenuItem "No notifications" False Nothing Nothing (pure ())] else map entry recent)
            ++ [ MenuSeparator
               , MenuItem "Do Not Disturb" True (Just (nsDnd st)) Nothing toggleDnd
               , menuItem "Show the Last One Again" (void (makoctl ["restore"]) >> safely refresh)
               , menuItem "Dismiss All" (void (makoctl ["dismiss", "--all"]) >> safely refresh) ]
        _ -> pure ()
  pure $ \_ -> do
    st <- readIORef ref
    let hover = notifHover cfg st
    pure [ (block (notifLabel cfg st))
             { blockOnClick = Just click
             , blockHoverMarkup = if null hover then Nothing else Just hover } ]

-- | Output of a makoctl command ("" if mako is not running).
makoctl :: [String] -> IO String
makoctl args = do
  r <- E.try (readProcessWithExitCode "makoctl" args "")
  pure $ case r of
    Right (ExitSuccess, out, _) -> out
    Right _ -> ""
    Left e -> const "" (e :: E.SomeException)

-- | @makoctl list -j@ / @history -j@: a JSON array of notifications (older
-- makos wrap it in @{"data":[[...]]}@).
parseList :: String -> [Notification]
parseList s = case J.parse s of
  Just (J.JArr xs) -> mapMaybe one xs
  Just v | Just (J.JArr [J.JArr xs]) <- J.lookupKey "data" v -> mapMaybe one xs
  _ -> []
  where
    one v = do
      J.JNum i <- field "id" v
      pure Notification
        { notifId = round i
        , notifApp = text "app_name" v
        , notifSummary = text "summary" v
        , notifBody = text "body" v
        , notifUrgent = text "urgency" v == "critical" }
    field k v = J.lookupKey k v >>= \x -> case x of
      J.JObj [("data", d)] -> Just d     -- old format: {"type":..., "data":...}
      _ -> Just x
    text k v = case field k v of
      Just (J.JStr t) -> t
      _ -> ""
