module Paths_seashells (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/cguo/Developer/seashells/.cabal-sandbox/bin"
libdir     = "/Users/cguo/Developer/seashells/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/seashells-0.1.0.0"
datadir    = "/Users/cguo/Developer/seashells/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/seashells-0.1.0.0"
libexecdir = "/Users/cguo/Developer/seashells/.cabal-sandbox/libexec"
sysconfdir = "/Users/cguo/Developer/seashells/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "seashells_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "seashells_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "seashells_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "seashells_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "seashells_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
