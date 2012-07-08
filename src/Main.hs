module Main (main) where

-- Standard modules
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (interceptWith)
import Network.WebSockets(defaultWebSocketsOptions)
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Data.Maybe (isNothing)
import qualified System.Environment
import qualified System.Exit 
import qualified System.Directory

-- Application modules
import WebApp
import WebsocketApp
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore as STM.FileStore
import qualified Observer.WatchDirectory
import qualified STM.Messages as STM (Messages, ServerMessages)
import qualified STM.Messages
import qualified STM.Clients
import ServerState
import MessageDispatch

-- Application entry point
main :: IO ()
main = do
  -- Read command line arguments
  args <- System.Environment.getArgs
  [watchPath, execPath] <- if length args == 0
    then putStrLn "No arguments supplied, using the current path as the watch directory." 
      >> System.Directory.getCurrentDirectory >>= \dir -> return [dir, defaultExecPath]
    else if head args == "--help"
      then printUsage >> System.Exit.exitSuccess
      else return $ if length args == 2 then args else [head args, defaultExecPath]
  
  -- Instantiate shared resources
  clients <- STM.Clients.newIO
  fileStore <- STM.FileStore.newIO watchPath
  serverMessages <- STM.Messages.newIO :: IO STM.ServerMessages
  clientMessages <- STM.Messages.newIO :: IO STM.Messages
  serverStateT <- newTVarIO Active :: IO (TVar ServerState)
    
  -- Run asynchronous observer: Watch directory
  maybeObserverId <- Observer.WatchDirectory.forkObserver fileStore serverMessages
  case maybeObserverId of
    Just observerId -> do
      -- Dispatch messages
      _ <- forkIO $ loopDispatch serverStateT clients fileStore serverMessages clientMessages
      -- Run the front controllers
      Warp.runSettings (webAppSettings clients fileStore serverMessages clientMessages) webApp
      -- Terminate dispatcher elegantly (wait until message queues are empty) 
      atomically $ writeTVar serverStateT Terminating
      waitUntilTerminated serverStateT
      -- Stop the asynchronous observers
      Observer.WatchDirectory.killObserver observerId
    Nothing ->
      System.Exit.exitWith $ System.Exit.ExitFailure 1
  
  -- Try to find the executable tool specified in the arguments
  maybeAbsExecPath <- System.Directory.findExecutable execPath
  if isNothing maybeAbsExecPath
    then putStrLn ("Executable '" ++ execPath ++ "' could not be found. The executable will not be run.")
    else return ()
  
  -- Run asynchronous observer: Watch executable
  -- TODO...
  
  where
    defaultExecPath = "chomp"
    printUsage = putStrLn "USAGE: chomp-test-inspector [watchPath] [executablePath]"

-- Loop the dispatch method until the termination flag is true and the message queues are both empty
loopDispatch :: TVar ServerState -> Clients -> STM.FileStore -> STM.ServerMessages -> STM.Messages -> IO ()
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

-- Yield to other threads until the server state is terminated 
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

-- Set up the web application (front controller) with the websocket application (front controller)
-- and shared resources (the file store and incoming message queues)
webAppSettings :: Clients -> STM.FileStore -> STM.ServerMessages -> STM.Messages -> Warp.Settings
webAppSettings clients fileStore serverMessages clientMessages = Warp.defaultSettings
  { Warp.settingsPort = 8080
  , Warp.settingsIntercept = interceptWith defaultWebSocketsOptions $ websocketApp clients fileStore serverMessages clientMessages
  }
