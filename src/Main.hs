module Main (main) where

-- Standard modules
import qualified Network.Wai.Handler.Warp as Warp
import Control.Concurrent (forkIO, yield)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import qualified System.Environment
import qualified System.Exit 
import qualified System.Directory

-- Application modules
import IOUtil
import WebApp
import WebsocketApp
import qualified FileStore
import FileStore (FileStore)
import qualified Observer.WatchDirectory
import qualified Observer.WatchExecutable
import qualified STM.Messages as STM (NetworkMessages, ServerMessages)
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
      >> System.Directory.getCurrentDirectory 
      >>= \dir -> return [dir, defaultExecPath]
    else if head args == "--help"
      then printUsage >> System.Exit.exitSuccess
      else return $ if length args == 2 then args else [head args, defaultExecPath]
  
  -- Instantiate shared resources
  clients <- STM.Clients.newIO
  fileStore <- FileStore.newIO watchPath
  serverMessages <- STM.Messages.newIO :: IO STM.ServerMessages
  clientMessages <- STM.Messages.newIO :: IO STM.NetworkMessages
  serverStateT <- newTVarIO Active :: IO (TVar ServerState)
  clientIdCounterT <- newTVarIO 1
  let appState = WebsocketAppState {
      appClients = clients,
      appFileStore = fileStore,
      appServerMessages = serverMessages,
      appClientMessages = clientMessages,
      appServerState = serverStateT,
      appClientIdCounter = clientIdCounterT
    }
  
  -- Run asynchronous observer: Watch directory
  maybeWatchDirectoryHandle <- Observer.WatchDirectory.forkObserver fileStore serverMessages
  watchDirectoryHandle <- case maybeWatchDirectoryHandle of
    Just handle -> return handle
    Nothing -> System.Exit.exitWith $ System.Exit.ExitFailure 1
  
  -- Try to find the executable tool specified in the arguments
  maybeAbsExecPath <- System.Directory.findExecutable execPath
  -- Run asynchronous observer: Watch executable
  maybeWatchExecutableHandle <- case maybeAbsExecPath of
    Nothing -> putStrLn ("Executable '" ++ execPath ++ "' could not be found. The executable will not be run.")
      >> return Nothing
    Just absExecPath -> Observer.WatchExecutable.forkObserver serverMessages absExecPath
  
  -- Dispatch messages
  _ <- forkIO $ loopDispatch serverStateT clients fileStore serverMessages clientMessages maybeAbsExecPath
  
  -- Run the front controllers
  Warp.runSettings (webAppSettings appState) webApp
  
  -- Stop the asynchronous observers
  case maybeWatchExecutableHandle of
    Just handle -> Observer.WatchExecutable.killObserver handle
    Nothing -> return ()
  Observer.WatchDirectory.killObserver watchDirectoryHandle
  
  -- Terminate dispatcher elegantly (wait until message queues are empty) 
  atomically $ writeTVar serverStateT Terminating
  waitUntilTerminated serverStateT
  where
    defaultExecPath = "chomp"
    printUsage = putStrLn "USAGE: chomp-test-inspector [watchPath] [executablePath]"

-- Loop the dispatch method until the termination flag is true and the message queues are both empty
loopDispatch :: TVar ServerState -> Clients -> FileStore -> STM.ServerMessages -> STM.NetworkMessages -> Maybe FilePath -> IO ()
loopDispatch serverStateT clients fileStore serverMessages clientMessages maybeExecPath = loop
  where
    loop = do
      messagesAvailable <- dispatch serverStateT clients fileStore serverMessages clientMessages maybeExecPath
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
  (flip foreverUntilIO) yield $ do
    state <- readTVarIO serverStateT
    return $ state == Terminated

-- Set up the web application (front controller) with the websocket application (front controller)
-- and shared resources (the file store and incoming message queues)
webAppSettings :: WebsocketAppState -> Warp.Settings
webAppSettings appState = Warp.defaultSettings
  { Warp.settingsPort = 8080
  , Warp.settingsIntercept = websocketIntercept appState 
  }
