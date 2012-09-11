module Handler.ServerHandler (handler) where

-- System modules
import Data.Maybe (isJust, fromJust)
import qualified System.FilePath
import qualified System.IO
import Control.Monad (liftM)

-- Application modules
import Message
import qualified FileStore
import FileStore (FileStore)
import WebsocketApp (Clients)
import qualified STM.Clients
import qualified STM.Messages as STM (ServerMessages)
import qualified Observer.WatchFile
import qualified Observer.WatchExecutable

handler :: FileStore -> STM.ServerMessages -> Clients -> Maybe FilePath -> ServerMessage -> IO ()
handler fs sm c maybeExecPath message = case message of

  -- Notify clients of log messages
  ServerNotify notification -> 
    (STM.Clients.broadcastMessage c $ Notify notification)

  -- Reload all files (or none)
  ServerReloadFiles event files ->
    FileStore.reloadIO fs files
    >> (STM.Clients.broadcastMessage c $ ReloadFiles event files)
    >> (Observer.WatchFile.loadFilesContents sm (FileStore.rootPath fs) files)
    >> if isJust maybeExecPath 
      then Observer.WatchExecutable.runEach
        sm 
        (fromJust maybeExecPath) 
        (FileStore.rootPath fs) $ 
          filter ((== ".source") . System.FilePath.takeExtension) files
      else return ()
    
  -- Load a file
  ServerLoadFile event file -> do
    -- Use the file store to determine whether the file is merely being replaced (thus modified)
    alreadyLoaded <- liftM not $ FileStore.loadIO fs file
    if alreadyLoaded 
      then return ()
      else (STM.Clients.broadcastMessage c $ LoadFile event file)
        >> (Observer.WatchFile.loadFileContents sm (FileStore.rootPath fs) file)
        >> if System.FilePath.takeExtension file == ".source" && isJust maybeExecPath 
          then Observer.WatchExecutable.run sm (fromJust maybeExecPath) (FileStore.rootPath fs) file
          else return ()

  -- Unload a file
  ServerUnloadFile event file -> 
    FileStore.unloadIO fs file
    >> (STM.Clients.broadcastMessage c $ UnloadFile event file)
  
  -- Load a file's contents
  ServerLoadFileContents file fileContents ->
    FileStore.loadCacheIO fs file (FileStore.FileInfo { FileStore.revision = 0, FileStore.operations = [] }) fileContents 
    >> (STM.Clients.broadcastMessage c $ LoadFileContents file 0 fileContents)
  
  -- Modified a file
  ServerLoadModifications file ->
    Observer.WatchFile.loadFileModifications sm fs file

  -- Broadcast operational transforms to the clients
  ServerOperationalTransform file rev actions ->
    -- Broadcast operations to clients
    (STM.Clients.broadcastMessage c $ OperationalTransform file rev actions)
  
  -- Execute the tool on all files
  ServerExecuteAll -> do
    files <- FileStore.allFilesIO fs
    Observer.WatchExecutable.runEach
      sm
      (fromJust maybeExecPath) 
      (FileStore.rootPath fs) $ 
        filter ((== ".source") . System.FilePath.takeExtension) files

  -- Unknown message
  _ -> System.IO.hPutStrLn System.IO.stderr $ "Unhandled server message: " ++ show message

