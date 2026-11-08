-- | Finding, compiling and launching the user configuration
-- (@~/.config/waymobar/waymobar.hs@), the same way xmonad does.
module Waymobar.Recompile
  ( Dirs (..)
  , getDirs
  , recompile
  ) where

import qualified Control.Exception as E
import Control.Concurrent (forkIO)
import Control.Monad (filterM, forM, void)
import Data.List (isSuffixOf)
import System.Directory
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (arch, os)
import System.IO (hPutStrLn, stderr)
import System.Process

data Dirs = Dirs
  { dirConfig :: FilePath   -- ^ ~/.config/waymobar
  , dirCache  :: FilePath   -- ^ ~/.cache/waymobar
  , cfgSource :: FilePath   -- ^ ~/.config/waymobar/waymobar.hs
  , cfgBinary :: FilePath   -- ^ ~/.cache/waymobar/waymobar-<arch>-<os>
  , cfgErrors :: FilePath   -- ^ ~/.cache/waymobar/waymobar.errors
  }

getDirs :: IO Dirs
getDirs = do
  cfg <- getXdgDirectory XdgConfig "waymobar"
  cache <- getXdgDirectory XdgCache "waymobar"
  pure Dirs
    { dirConfig = cfg
    , dirCache = cache
    , cfgSource = cfg </> "waymobar.hs"
    , cfgBinary = cache </> ("waymobar-" ++ arch ++ "-" ++ os)
    , cfgErrors = cache </> "waymobar.errors"
    }

-- | Compile the configuration if it changed (or always when forced).
-- Returns False only if there is a configuration and it failed to build.
--
-- If @~/.config/waymobar/build@ exists and is executable it is used instead
-- of ghc, with the output binary path as its only argument (handy for cabal or
-- stack based setups).
recompile :: Bool -> IO Bool
recompile force = do
  d <- getDirs
  haveSrc <- doesFileExist (cfgSource d)
  if not haveSrc
    then pure True
    else do
      createDirectoryIfMissing True (dirCache d)
      let buildScript = dirConfig d </> "build"
      useScript <- doesFileExist buildScript >>= \e ->
        if e then executable <$> getPermissions buildScript else pure False
      libs <- hsFiles (dirConfig d </> "lib")
      -- also rebuild when waymobar itself was reinstalled (new library)
      self <- getExecutablePath
      stale <- needsBuild (cfgBinary d) (cfgSource d : buildScript : self : libs)
      if not (force || stale)
        then pure True
        else do
          let cp
                | useScript = (proc buildScript [cfgBinary d]) { cwd = Just (dirConfig d) }
                | otherwise = (proc "ghc"
                    [ "--make", "waymobar.hs", "-i", "-ilib", "-fforce-recomp"
                    , "-main-is", "main", "-threaded", "-v0"
                    , "-outputdir", dirCache d </> ("build-" ++ arch ++ "-" ++ os)
                    , "-o", cfgBinary d ]) { cwd = Just (dirConfig d) }
          r <- E.try (readCreateProcessWithExitCode cp "")
          case r of
            Right (ExitSuccess, _, _) -> do
              writeFile (cfgErrors d) ""
              pure True
            Right (ExitFailure _, out, err) -> failed d (out ++ err)
            Left e -> failed d (show (e :: E.IOException))
  where
    failed d msg = do
      writeFile (cfgErrors d) msg
      hPutStrLn stderr ("waymobar: recompilation failed:\n" ++ msg)
      notify ("Error compiling " ++ cfgSource d ++ "\n\n" ++ take 800 msg)
      pure False

needsBuild :: FilePath -> [FilePath] -> IO Bool
needsBuild bin srcs = do
  haveBin <- doesFileExist bin
  if not haveBin
    then pure True
    else do
      binTime <- getModificationTime bin
      existing <- filterM doesFileExist srcs
      times <- mapM getModificationTime existing
      pure (any (> binTime) times)

hsFiles :: FilePath -> IO [FilePath]
hsFiles dir = do
  isDir <- doesDirectoryExist dir
  if not isDir
    then pure []
    else do
      entries <- map (dir </>) <$> listDirectory dir
      fmap concat $ forM entries $ \e -> do
        sub <- doesDirectoryExist e
        if sub then hsFiles e else pure [e | ".hs" `isSuffixOf` e]

-- | Best-effort desktop notification.
notify :: String -> IO ()
notify msg = do
  have <- findExecutable "notify-send"
  case have of
    Nothing -> pure ()
    Just ns -> do
      r <- E.try (spawnProcess ns ["-u", "critical", "waymobar", msg])
      case r of
        Right ph -> void (forkIO (void (waitForProcess ph)))
        Left e -> hPutStrLn stderr ("waymobar: notify-send: " ++ show (e :: E.IOException))
