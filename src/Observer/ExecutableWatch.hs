--{-# LANGUAGE OverloadedStrings, CPP #-}
module Observer.ExecutableWatch (ExecutableWatchHandle) where

-- Standard modules
import System.INotify (INotify, EventVariety(..), Event(..), initINotify, killINotify, addWatch)
#ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.Exception as Exception
#endif

-- Types
type ExecutableWatchHandle = INotify
