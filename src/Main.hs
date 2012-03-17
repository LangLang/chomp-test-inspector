{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.IO (stdout, hFlush)
import Data.Text hiding (map)
import Data.Text.IO (putStrLn, hPutStrLn)
import Prelude hiding (putStrLn)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (interceptWith)
import Network.WebSockets(defaultWebSocketsOptions)
import System.INotify (initINotify, killINotify, addWatch, removeWatch, EventVariety(..), Event)

import WebApp
import WebsocketApp

webAppSettings = defaultSettings
  { settingsPort = 8080
  , settingsIntercept = interceptWith defaultWebSocketsOptions websocketApp
  }

main :: IO ()
main = do
  inotify <- initINotify
  addWatch inotify [Modify] "tests/test00.source" sourceFileChanged
  Warp.runSettings webAppSettings webApp
  killINotify inotify
  where
    sourceFileChanged :: Event -> IO ()
    sourceFileChanged e = do
      putStrLn "Source file changed"
      return ()
