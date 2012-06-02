module Handler.StorageHandler (reloadFiles, loadFile) where

-- Application modules
import Message
import FileStore
import WebsocketApp (Clients)
import STM.Clients hiding (Clients)
import qualified STM.FileStore as STM (FileStore)

reloadFiles :: Clients -> STM.FileStore -> [FileInfo] -> IO ()
reloadFiles clients fileStore fileInfos = do
  -- TODO: atomically clear the fileStore and set it to the incoming files
  broadcastMessage clients $ ReloadFiles fileInfos 

loadFile :: Clients -> STM.FileStore -> FileInfo -> IO ()
loadFile clients fileStore fileInfo = do
  -- TODO: atomically add the file to the fileStore 
  broadcastMessage clients $ LoadFile fileInfo

