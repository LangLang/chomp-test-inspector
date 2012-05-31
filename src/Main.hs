module Main (main) where

-- Standard modules
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (interceptWith)
import Network.WebSockets(defaultWebSocketsOptions)
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)

-- Application modules
import WebApp
import WebsocketApp
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore as STM.FileStore
import FileObserver
import qualified STM.Messages as STM (Messages)
import qualified STM.Messages
import qualified STM.Clients
import ServerState
import MessageDispatch

-- Application entry point
main :: IO ()
main = do
  -- Instantiate shared resources
  clients <- STM.Clients.newIO
  fileStore <- STM.FileStore.newIO
  serverMessages <- STM.Messages.newIO
  clientMessages <- STM.Messages.newIO
  serverStateT <- newTVarIO Active :: IO (TVar ServerState)
  -- Run asynchronous observers
  maybeId <- forkFileObserver watchPath fileStore serverMessages 
  case maybeId of
    Just observerId -> do
      -- Dispatch messages
      _ <- forkIO $ loopDispatch serverStateT clients fileStore serverMessages clientMessages
      -- Run the front controllers
      Warp.runSettings (webAppSettings clients fileStore serverMessages clientMessages) webApp
      -- Terminate dispatcher elegantly (wait until message queues are empty) 
      atomically $ writeTVar serverStateT Terminating
      waitUntilTerminated serverStateT
      -- Stop the asynchronous observers
      killFileObserver observerId
    Nothing ->
      return ()
  where
    watchPath = "tests" :: FilePath

-- Loop the dispatch method until the termination flag is true and the message queues are both empty
loopDispatch :: TVar ServerState -> Clients -> STM.FileStore -> STM.Messages -> STM.Messages -> IO ()
loopDispatch serverStateT clients fileStore serverMessages clientMessages = loop
  where
    loop = do
      messagesAvailable <- dispatch serverStateT clients fileStore serverMessages clientMessages
      if messagesAvailable
        then loop
        else do
          state <- readTVarIO serverStateT
          if state == Terminated
            then return ()
            else loop

waitUntilTerminated :: TVar ServerState -> IO ()
waitUntilTerminated serverStateT =
  foreverUntilIO yield $ do
    state <- readTVarIO serverStateT
    return $ state == Terminated 

-- Repeat an IO action until another IO operation returns true 
foreverUntilIO :: IO () -> IO Bool -> IO ()
foreverUntilIO loop check = do
  _ <- loop
  cond <- check
  if cond
    then return ()
    else foreverUntilIO loop check

{-
-- A helper that lets us end a thread (or any repeating monad) elegantly (without interupting it)
foreverUntilT :: forall (m::* -> *) a b. Monad m => TVar Bool ->  IO a -> IO ()  
foreverUntilT terminate = do
  finished <- readTVarIO terminate
  if finished
    then return ()
    else do
      forever
      foreverUntilT
-}

-- Set up the web application (front controller) with the websocket application (front controller)
-- and shared resources (the file store and incoming message queues)
webAppSettings :: Clients -> STM.FileStore -> STM.Messages -> STM.Messages -> Warp.Settings
webAppSettings clients fileStore serverMessages clientMessages = Warp.defaultSettings
  { Warp.settingsPort = 8080
  , Warp.settingsIntercept = interceptWith defaultWebSocketsOptions $ websocketApp clients fileStore serverMessages clientMessages
  }
