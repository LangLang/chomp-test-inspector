module Handler.StorageHandler (reloadWatchPath, loadFile) where

-- Standard modules
import System.IO.Error (try)

-- Application modules
import Message
import WebsocketApp (Clients)
import qualified STM.Clients
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore

reloadWatchPath :: Clients -> STM.FileStore -> IO ()
reloadWatchPath clients fileStore = do
  errorOrFiles <- try $ STM.FileStore.reload fileStore
  STM.Clients.broadcastMessage clients $ case errorOrFiles of
    Left _ -> ReloadFiles MovedOutRootDirectory []
    Right files -> ReloadFiles RestoredRootDirectory files

loadFile :: Clients -> STM.FileStore -> Message -> IO ()
loadFile clients fileStore message = do
  -- TODO: atomically add the file to the fileStore 
  STM.Clients.broadcastMessage clients message
