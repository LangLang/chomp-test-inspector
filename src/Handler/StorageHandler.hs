module Handler.StorageHandler (handler) where

-- System modules
import Data.Maybe (isJust, fromJust)
import qualified System.FilePath
import qualified System.IO

-- Application modules
import Message
import WebsocketApp (Clients)
import qualified STM.Clients
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore
import qualified STM.Messages as STM (ServerMessages)
import qualified Observer.WatchFile
import qualified Observer.WatchExecutable

handler :: STM.FileStore -> STM.ServerMessages -> Clients -> Maybe FilePath -> ServerMessage -> IO ()
handler fs sm c maybeExecPath message = case message of

  -- Reload all files (or none)
  ServerReloadFiles event files ->
    STM.FileStore.reload fs files
    >> (STM.Clients.broadcastMessage c $ ReloadFiles event files)
    >> (Observer.WatchFile.loadFilesContents sm (STM.FileStore.rootPath fs) files)
    >> if isJust maybeExecPath 
      then Observer.WatchExecutable.runEach
        sm 
        (fromJust maybeExecPath) 
        (STM.FileStore.rootPath fs) $ 
          filter ((== ".source") . System.FilePath.takeExtension) files
      else return ()
    
  -- Load a file
  ServerLoadFile event file ->
    STM.FileStore.load fs file
    >> (STM.Clients.broadcastMessage c $ LoadFile event file)
    >> (Observer.WatchFile.loadFileContents sm (STM.FileStore.rootPath fs) file)
    >> if System.FilePath.takeExtension file == ".source" && isJust maybeExecPath 
      then Observer.WatchExecutable.run sm (fromJust maybeExecPath) (STM.FileStore.rootPath fs) file
      else return ()

  -- Load a file's contents
  ServerLoadFileContents file fileContents ->
    STM.FileStore.loadContents fs file fileContents
    >> (STM.Clients.broadcastMessage c $ LoadFileContents file $ Just fileContents)
  
  -- Unload a file
  ServerUnloadFile event file -> 
    STM.FileStore.unload fs file
    >> (STM.Clients.broadcastMessage c $ UnloadFile event file)
  
  -- Modified a file
  ServerLoadModifications file ->
    Observer.WatchFile.loadFileModifications sm fs file
  
  -- Execute the tool on all files
  ServerExecuteAll -> do
    files <- STM.FileStore.allFiles fs
    Observer.WatchExecutable.runEach
      sm
      (fromJust maybeExecPath) 
      (STM.FileStore.rootPath fs) $ 
        filter ((== ".source") . System.FilePath.takeExtension) files
        
  -- Notify clients of log messages
  ServerNotify notification -> (STM.Clients.broadcastMessage c $ Notify notification)

  -- Unknown message
  _ -> System.IO.hPutStrLn System.IO.stderr $ "Unhandled server message: " ++ show message

--loadDiff :: Clients -> STM.FileStore -> FileInfo -> IO ()
--loadDiff clients fileStore file =
--  STM.Clients.broadcastMessage clients $ LoadDiff generateDiff
--  where
--    generateDiff :: IO Patch
--generateDiffPatch    generateDiff = 
