module Handler.StorageHandler (reloadWatchPath, loadFile) where

-- Standard modules
import System.IO.Error (try)

-- Application modules
import Message
import WebsocketApp (Clients)
import qualified STM.Clients
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore
import qualified STM.Messages as STM (Messages)
import qualified STM.Messages

reloadWatchPath :: Clients -> STM.FileStore -> STM.Messages -> IO ()
reloadWatchPath clients fileStore clientMessages = do
  -- Try to reload the file store
  errorOrFiles <- try $ STM.FileStore.reload fileStore
  -- Clear all of the unprocessed client messages if the root directory was moved out
  -- TODO: We should also ignore all message we receive from any client until they acknowledge
  --       a Reload message with the "RestoredRootDirectory" event...
  case errorOrFiles of
    Left _ -> STM.Messages.clearMessages clientMessages
    Right _ -> return ()
  -- Broadcast a reload command to all of the clients 
  STM.Clients.broadcastMessage clients $ case errorOrFiles of
    Left _ -> ReloadFiles MovedOutRootDirectory []
    Right files -> ReloadFiles RestoredRootDirectory files

loadFile :: Clients -> STM.FileStore -> Message -> IO ()
loadFile clients fileStore message = do
  -- TODO: atomically add the file to the fileStore 
  STM.Clients.broadcastMessage clients message
