--{-# LANGUAGE OverloadedStrings, CPP #-}
module Observer.WatchExecutable (WatchExecutableHandle) where

-- Standard modules
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
#ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.Exception as Exception
#endif

-- Types
type WatchExecutableHandle = INotify
