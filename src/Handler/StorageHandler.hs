module Handler.StorageHandler (reloadFiles, loadFile) where

-- Application modules
import Message
import FileStore
import WebsocketApp (Clients)
import STM.Clients hiding (Clients)
import qualified STM.FileStore as STM (FileStore)

reloadFiles :: Clients -> STM.FileStore -> Message -> IO ()
reloadFiles clients fileStore message = do
  -- TODO: atomically clear the fileStore and set it to the incoming files
  broadcastMessage clients message 

loadFile :: Clients -> STM.FileStore -> Message -> IO ()
loadFile clients fileStore message = do
  -- TODO: atomically add the file to the fileStore 
  broadcastMessage clients message

