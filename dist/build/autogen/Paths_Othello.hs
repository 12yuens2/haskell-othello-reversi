module Paths_Othello (
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

bindir     = "/cs/home/imb3/.cabal/bin"
libdir     = "/cs/home/imb3/.cabal/lib/Othello-0.1.0.0/ghc-7.6.3"
datadir    = "/cs/home/imb3/.cabal/share/Othello-0.1.0.0"
libexecdir = "/cs/home/imb3/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Othello_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Othello_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Othello_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Othello_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
