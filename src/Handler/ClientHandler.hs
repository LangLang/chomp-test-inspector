module Handler.ClientHandler (handler) where

-- System modules
--import Data.Maybe (isJust, fromJust)
--import qualified System.FilePath
import qualified System.IO
--import Control.Monad (liftM)

-- Application modules
import Message
--import qualified FileStore
import FileStore (FileStore)
import WebsocketApp (Clients)
import qualified STM.Clients
import qualified STM.Messages as STM (ServerMessages)
import qualified Observer.WatchFile

handler :: FileStore -> STM.ServerMessages -> Clients -> Message -> IO ()
handler fs sm c message = case message of
  -- Apply operational transform sent by the client
  OperationalTransform file rev actions ->
    -- Apply changes to the file store
    Observer.WatchFile.applyOperation sm fs file rev actions
    -- Broadcast operations to clients
    -- (STM.Clients.broadcastMessage c $ OperationalTransform file rev actions)
  
  -- Unknown message
  _ -> System.IO.hPutStrLn System.IO.stderr $ "Unhandled client message: " ++ show message

