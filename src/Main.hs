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
import qualified STM.Messages as STM (Messages)
import qualified STM.Messages (newIO)

-- Application entry point
main :: IO ()
main = do
  -- Instantiate shared resources
  fileStore <- STM.FileStore.newIO
  serverMessages <- STM.Messages.newIO
  clientMessages <- STM.Messages.newIO
  -- Run asynchronous observers
  maybeId <- forkFileObserver watchPath fileStore serverMessages 
  case maybeId of
    Just observerId -> do
      -- Run the front controllers
      Warp.runSettings (webAppSettings fileStore serverMessages clientMessages) webApp
      -- Stop the asynchronous observers
      killFileObserver observerId
    Nothing ->
      return ()
  where
    watchPath = "tests" :: FilePath

-- Set up the web application (front controller) with the websocket application (front controller)
-- and shared resources (the file store and incoming message queues)
webAppSettings :: STM.FileStore -> STM.Messages -> STM.Messages -> Warp.Settings
webAppSettings fileStore serverMessages clientMessages = Warp.defaultSettings
  { Warp.settingsPort = 8080
  , Warp.settingsIntercept = interceptWith defaultWebSocketsOptions $ websocketApp fileStore serverMessages clientMessages
  }
