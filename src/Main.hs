{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- Standard modules
import System.IO (stdout, hFlush)
import Data.Text hiding (map)
import Data.Text.IO (putStrLn, hPutStrLn)
import Prelude hiding (putStrLn)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (interceptWith)
import Network.WebSockets(defaultWebSocketsOptions)
import System.INotify (initINotify, killINotify, addWatch, removeWatch, EventVariety(..), Event)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)

-- Application modules
import WebApp
import WebsocketApp
import FileStore

-- Main application entry point
main :: IO ()
main = do
  inotify <- initINotify
  addWatch inotify [Modify] "tests" sourceFileChanged
  fileStore <- newMVar []
  Warp.runSettings (webAppSettings fileStore) webApp
  killINotify inotify
  where
    sourceFileChanged :: Event -> IO ()
    sourceFileChanged e = do
      putStrLn "Source file changed"
      return ()

-- Set up the web application with the websocket app and a thread-safe file store
webAppSettings :: MVar FileStore -> Warp.Settings
webAppSettings fileStore = defaultSettings
  { settingsPort = 8080
  , settingsIntercept = interceptWith defaultWebSocketsOptions $ websocketApp fileStore
  }
