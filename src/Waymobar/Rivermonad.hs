-- | Client for rivermonad's IPC socket: a background thread follows the
-- @"EventStream"@ (reconnecting whenever rivermonad restarts) and actions
-- are sent on short-lived connections.
module Waymobar.Rivermonad
  ( RState (..)
  , Workspace (..)
  , Action (..)
  , RivermonadClient
  , startClient
  , currentState
  , sendAction
  , socketPath
  , parseState
  ) where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Environment (lookupEnv)
import System.Directory (doesPathExist)
import System.FilePath ((</>))

import qualified Waymobar.JSON as J

-- | What rivermonad publishes on every change.
data RState = RState
  { rsWorkspaces    :: [Workspace]
  , rsFocusedOutput :: Maybe String
  , rsTitle         :: Maybe String
  , rsAppId         :: Maybe String
  } deriving (Eq, Show)

data Workspace = Workspace
  { wsName    :: String
  , wsIndex   :: Int
  , wsOutput  :: Maybe String  -- ^ the output showing it, if any
  , wsFocused :: Bool          -- ^ the focused workspace (on the focused output)
  , wsVisible :: Bool          -- ^ shown on some output
  , wsWindows :: Int
  , wsLayout  :: String
  , wsExtra   :: Bool          -- ^ not in the configured workspace list
  } deriving (Eq, Show)

-- | The actions rivermonad accepts over IPC.
data Action
  = FocusWorkspace String  -- ^ xmonad's @view@
  | ViewWorkspace String   -- ^ xmonad's @greedyView@
  | NextLayout
  | JumpToLayout String
  | FocusUp
  | FocusDown
  | Restart
  deriving (Eq, Show)

data RivermonadClient = RivermonadClient
  { rcState :: IORef (Maybe RState)
  }

-- | The socket of the rivermonad managing /this/ Wayland display, if it
-- exists; @RIVERMONAD_SOCKET@ only as a fallback. The variable alone is
-- not trusted: in a session nested inside another rivermonad it is
-- inherited from the outer one.
socketPath :: IO FilePath
socketPath = do
  dir <- fromMaybe "/tmp" <$> lookupEnv "XDG_RUNTIME_DIR"
  disp <- fromMaybe "wayland-0" <$> lookupEnv "WAYLAND_DISPLAY"
  let own = dir </> ("rivermonad-" ++ map (\c -> if c == '/' then '_' else c) disp ++ ".sock")
  ownExists <- doesPathExist own
  env <- lookupEnv "RIVERMONAD_SOCKET"
  pure $ case env of
    Just p | not ownExists, not (null p) -> p
    _ -> own

-- | Start following rivermonad. @changed@ runs after every new state
-- (and when the connection is lost, with the state reset to 'Nothing').
startClient :: IO () -> IO RivermonadClient
startClient changed = do
  ref <- newIORef Nothing
  failures <- newIORef (0 :: Int)
  void $ forkIO $ forever $ do
    void (E.try (follow ref failures) :: IO (Either E.SomeException ()))
    -- Keep showing the last state while rivermonad restarts (mod-q takes
    -- about a second); only clear it once it has been gone for ~3 s.
    n <- atomicModifyIORef' failures (\k -> (k + 1, k + 1))
    when (n >= 10) $ do
      had <- atomicModifyIORef' ref (\s -> (Nothing, s))
      case had of
        Just _ -> changed
        Nothing -> pure ()
    threadDelay 300000
  pure (RivermonadClient ref)
  where
    follow ref failures = do
      path <- socketPath
      E.bracket (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \s -> do
        NS.connect s (NS.SockAddrUnix path)
        writeIORef failures 0
        NSB.sendAll s (BC.pack "\"EventStream\"\n")
        let loop buf = do
              chunk <- NSB.recv s 65536
              if B.null chunk
                then pure ()
                else do
                  let (ls, rest) = splitLines (buf <> chunk)
                  case mapMaybe parseState ls of
                    [] -> pure ()
                    sts -> do
                      writeIORef ref (Just (last sts))
                      changed
                  loop rest
        loop B.empty

splitLines :: B.ByteString -> ([B.ByteString], B.ByteString)
splitLines bs = case BC.elemIndex '\n' bs of
  Nothing -> ([], bs)
  Just i -> let (ls, rest) = splitLines (B.drop (i + 1) bs) in (B.take i bs : ls, rest)

currentState :: RivermonadClient -> IO (Maybe RState)
currentState = readIORef . rcState

-- | Send an action in the background.
sendAction :: Action -> IO ()
sendAction a = void $ forkIO $ void $ (E.try :: IO () -> IO (Either E.SomeException ())) $ do
  path <- socketPath
  E.bracket (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \s -> do
    NS.connect s (NS.SockAddrUnix path)
    NSB.sendAll s (J.renderUtf8 (J.obj [("Action", encode a)]) <> BC.pack "\n")
    void (NSB.recv s 4096)
  where
    encode = \case
      FocusWorkspace t -> J.obj [("FocusWorkspace", J.str t)]
      ViewWorkspace t -> J.obj [("ViewWorkspace", J.str t)]
      NextLayout -> J.str "NextLayout"
      JumpToLayout l -> J.obj [("JumpToLayout", J.str l)]
      FocusUp -> J.str "FocusUp"
      FocusDown -> J.str "FocusDown"
      Restart -> J.str "Restart"

-- | Parse one @{"State":{...}}@ line.
parseState :: B.ByteString -> Maybe RState
parseState line = do
  v <- J.parseUtf8 line
  st <- J.lookupKey "State" v
  J.JArr wss <- J.lookupKey "workspaces" st
  pure RState
    { rsWorkspaces = mapMaybe workspace wss
    , rsFocusedOutput = optStr "focused_output" st
    , rsTitle = optStr "title" st
    , rsAppId = optStr "app_id" st
    }
  where
    optStr k o = case J.lookupKey k o of
      Just (J.JStr s) -> Just s
      _ -> Nothing
    bool k o = J.lookupKey k o == Just (J.JBool True)
    num k o = case J.lookupKey k o of
      Just (J.JNum n) -> round n
      _ -> 0
    workspace o = do
      J.JStr name <- J.lookupKey "name" o
      pure Workspace
        { wsName = name
        , wsIndex = num "idx" o
        , wsOutput = optStr "output" o
        , wsFocused = bool "focused" o
        , wsVisible = bool "visible" o
        , wsWindows = num "windows" o
        , wsLayout = fromMaybe "" (optStr "layout" o)
        , wsExtra = bool "extra" o
        }
