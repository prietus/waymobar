-- | The @waymobar@ executable: like xmonad, it builds and runs
-- @~/.config/waymobar/waymobar.hs@ when there is one, and otherwise shows
-- the default bar.
module Waymobar.Main
  ( launch
  ) where

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)

import Waymobar.Bar (runBar)
import Waymobar.Config (def)
import Waymobar.Recompile

launch :: IO ()
launch = do
  args <- getArgs
  case args of
    ["--recompile"] -> do
      ok <- recompile True
      if ok then exitSuccess else exitFailure
    ["--default"] -> runBar def
    ["--help"] -> usage >> exitSuccess
    ["--version"] -> putStrLn "waymobar 0.1.0" >> exitSuccess
    [] -> do
      d <- getDirs
      have <- doesFileExist (cfgSource d)
      if not have
        then runBar def
        else do
          ok <- recompile False
          bin <- doesFileExist (cfgBinary d)
          when (not ok) $ hPutStrLn stderr "waymobar: using the last working build of the configuration"
          if bin
            then executeFile (cfgBinary d) False [] Nothing
            else do
              hPutStrLn stderr "waymobar: no build of the configuration, showing the default bar"
              runBar def
    _ -> usage >> exitFailure
  where
    usage = putStr $ unlines
      [ "Usage: waymobar [--recompile | --default | --version]"
      , ""
      , "Runs ~/.config/waymobar/waymobar.hs (rebuilt when it changes), or the"
      , "default bar when there is none."
      , ""
      , "  --recompile  build the configuration and exit"
      , "  --default    ignore the configuration"
      ]
