{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_app (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/run/media/vb/data/docs/foo/.stack-work/install/x86_64-linux-nix/085436eee16d2ec62ef7b34e9a65fd4815c4f061c82625db1472f1c3eebd2c15/9.4.7/bin"
libdir     = "/run/media/vb/data/docs/foo/.stack-work/install/x86_64-linux-nix/085436eee16d2ec62ef7b34e9a65fd4815c4f061c82625db1472f1c3eebd2c15/9.4.7/lib/x86_64-linux-ghc-9.4.7/app-0.1.0.0-6Oz8pPZ18i5EN4fUTun83F-app-exe"
dynlibdir  = "/run/media/vb/data/docs/foo/.stack-work/install/x86_64-linux-nix/085436eee16d2ec62ef7b34e9a65fd4815c4f061c82625db1472f1c3eebd2c15/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/run/media/vb/data/docs/foo/.stack-work/install/x86_64-linux-nix/085436eee16d2ec62ef7b34e9a65fd4815c4f061c82625db1472f1c3eebd2c15/9.4.7/share/x86_64-linux-ghc-9.4.7/app-0.1.0.0"
libexecdir = "/run/media/vb/data/docs/foo/.stack-work/install/x86_64-linux-nix/085436eee16d2ec62ef7b34e9a65fd4815c4f061c82625db1472f1c3eebd2c15/9.4.7/libexec/x86_64-linux-ghc-9.4.7/app-0.1.0.0"
sysconfdir = "/run/media/vb/data/docs/foo/.stack-work/install/x86_64-linux-nix/085436eee16d2ec62ef7b34e9a65fd4815c4f061c82625db1472f1c3eebd2c15/9.4.7/etc"

getBinDir     = catchIO (getEnv "app_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "app_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "app_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "app_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "app_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "app_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
