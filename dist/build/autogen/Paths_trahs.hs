module Paths_trahs (
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
version = Version {versionBranch = [1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/annag414/Library/Haskell/ghc-7.6.3/lib/trahs-1.0.0/bin"
libdir     = "/Users/annag414/Library/Haskell/ghc-7.6.3/lib/trahs-1.0.0/lib"
datadir    = "/Users/annag414/Library/Haskell/ghc-7.6.3/lib/trahs-1.0.0/share"
libexecdir = "/Users/annag414/Library/Haskell/ghc-7.6.3/lib/trahs-1.0.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "trahs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "trahs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "trahs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trahs_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
