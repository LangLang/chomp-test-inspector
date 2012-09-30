module Handler.ClientHandler (handler) where

-- System modules
--import Data.Maybe (isJust, fromJust)
--import qualified System.FilePath
import qualified System.IO
--import Control.Monad (liftM)

-- Application modules
import Message
import FileStore (FileStore)
import WebsocketApp (Clients)
import qualified STM.Messages as STM (ServerMessages)
import qualified STM.Messages
import qualified Observer.WatchFile

handler :: FileStore -> STM.ServerMessages -> Clients -> StampedNetworkMessage -> IO ()
handler fs sm c (StampedMessage cid t message) = case message of
  -- Apply operational transform sent by the client
  OperationalTransform file rev actions opId -> do
    -- Apply changes to the file store
    errorOrMessage <- Observer.WatchFile.applyOperation fs file rev actions opId
    case errorOrMessage of
      Left err -> System.IO.hPutStrLn System.IO.stderr err
      Right message' -> enqueue $ restamp message'
  
  -- Unknown message
  _ -> System.IO.hPutStrLn System.IO.stderr $ "Unhandled client message: " ++ show message
  
  where
    enqueue = STM.Messages.enqueue sm
    restamp = StampedMessage cid t