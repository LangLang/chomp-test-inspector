module Main (main) where

-- Standard modules
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (interceptWith)
import Network.WebSockets(defaultWebSocketsOptions)

-- Application modules
import WebApp
import WebsocketApp
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore as STM.FileStore
import FileObserver

-- Application entry point
main :: IO ()
main = do
  -- Instantiate shared resources
  fileStore <- STM.FileStore.newIO
  --serverMessages <- STM.Messages.newIO -- TODO
  -- Run asynchronous observers
  observerId <- forkFileObserver watchPath fileStore
  -- Run the front controllers
  Warp.runSettings (webAppSettings fileStore) webApp
  -- Stop the asynchronous observers
  killFileObserver observerId
  where
    watchPath = "tests" :: FilePath

-- Set up the web application (front controller) with the websocket application (front controller)
-- and shared resources (the file store and incoming message queues)
webAppSettings :: STM.FileStore -> Warp.Settings
webAppSettings fileStore = Warp.defaultSettings
  { Warp.settingsPort = 8080
  , Warp.settingsIntercept = interceptWith defaultWebSocketsOptions $ websocketApp fileStore
  }
