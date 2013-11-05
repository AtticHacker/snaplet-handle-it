module Paths_snaplet_handle_it (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/kevin/.cabal/bin"
libdir     = "/home/kevin/.cabal/lib/snaplet-handle-it-0.1.0.0/ghc-7.6.3"
datadir    = "/home/kevin/.cabal/share/snaplet-handle-it-0.1.0.0"
libexecdir = "/home/kevin/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "snaplet_handle_it_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snaplet_handle_it_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "snaplet_handle_it_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snaplet_handle_it_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
