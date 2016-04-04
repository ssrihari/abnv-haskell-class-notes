module Paths_Maze (
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

bindir     = "/Users/sriharisriraman/dev/haskell/abnv-course/haskell-maze/.stack-work/install/x86_64-osx/lts-5.8/7.10.3/bin"
libdir     = "/Users/sriharisriraman/dev/haskell/abnv-course/haskell-maze/.stack-work/install/x86_64-osx/lts-5.8/7.10.3/lib/x86_64-osx-ghc-7.10.3/Maze-0.1.0.0-0GOIvroSIRBL38fnwozFIJ"
datadir    = "/Users/sriharisriraman/dev/haskell/abnv-course/haskell-maze/.stack-work/install/x86_64-osx/lts-5.8/7.10.3/share/x86_64-osx-ghc-7.10.3/Maze-0.1.0.0"
libexecdir = "/Users/sriharisriraman/dev/haskell/abnv-course/haskell-maze/.stack-work/install/x86_64-osx/lts-5.8/7.10.3/libexec"
sysconfdir = "/Users/sriharisriraman/dev/haskell/abnv-course/haskell-maze/.stack-work/install/x86_64-osx/lts-5.8/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Maze_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Maze_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Maze_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Maze_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Maze_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
