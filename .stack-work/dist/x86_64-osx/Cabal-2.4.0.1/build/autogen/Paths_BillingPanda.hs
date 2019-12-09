{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_BillingPanda (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Frank/Haskell/Study/BillingPanda/.stack-work/install/x86_64-osx/24efbeac434506d2609f638b1fdb8bbc64484932bd8ae1bbf0903068b0b6dd52/8.6.5/bin"
libdir     = "/Users/Frank/Haskell/Study/BillingPanda/.stack-work/install/x86_64-osx/24efbeac434506d2609f638b1fdb8bbc64484932bd8ae1bbf0903068b0b6dd52/8.6.5/lib/x86_64-osx-ghc-8.6.5/BillingPanda-0.1.0.0-BSESPhKs1fjDIWuCVOyDGQ"
dynlibdir  = "/Users/Frank/Haskell/Study/BillingPanda/.stack-work/install/x86_64-osx/24efbeac434506d2609f638b1fdb8bbc64484932bd8ae1bbf0903068b0b6dd52/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/Frank/Haskell/Study/BillingPanda/.stack-work/install/x86_64-osx/24efbeac434506d2609f638b1fdb8bbc64484932bd8ae1bbf0903068b0b6dd52/8.6.5/share/x86_64-osx-ghc-8.6.5/BillingPanda-0.1.0.0"
libexecdir = "/Users/Frank/Haskell/Study/BillingPanda/.stack-work/install/x86_64-osx/24efbeac434506d2609f638b1fdb8bbc64484932bd8ae1bbf0903068b0b6dd52/8.6.5/libexec/x86_64-osx-ghc-8.6.5/BillingPanda-0.1.0.0"
sysconfdir = "/Users/Frank/Haskell/Study/BillingPanda/.stack-work/install/x86_64-osx/24efbeac434506d2609f638b1fdb8bbc64484932bd8ae1bbf0903068b0b6dd52/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "BillingPanda_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BillingPanda_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "BillingPanda_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "BillingPanda_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BillingPanda_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "BillingPanda_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
