module Paths_maze (
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

bindir     = "/Users/sriharisriraman/dev/haskell/abnv-course/maze/.stack-work/install/x86_64-osx/lts-5.4/7.10.3/bin"
libdir     = "/Users/sriharisriraman/dev/haskell/abnv-course/maze/.stack-work/install/x86_64-osx/lts-5.4/7.10.3/lib/x86_64-osx-ghc-7.10.3/maze-0.1.0.0-GoKnX7KbWw6L6IQg5IY6eg"
datadir    = "/Users/sriharisriraman/dev/haskell/abnv-course/maze/.stack-work/install/x86_64-osx/lts-5.4/7.10.3/share/x86_64-osx-ghc-7.10.3/maze-0.1.0.0"
libexecdir = "/Users/sriharisriraman/dev/haskell/abnv-course/maze/.stack-work/install/x86_64-osx/lts-5.4/7.10.3/libexec"
sysconfdir = "/Users/sriharisriraman/dev/haskell/abnv-course/maze/.stack-work/install/x86_64-osx/lts-5.4/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "maze_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "maze_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "maze_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "maze_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "maze_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
