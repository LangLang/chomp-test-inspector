module MessageDispatch (dispatch) where

-- TODO: Should this be called Scheduler?

-- Standard modules
import Control.Concurrent.STM (TVar, atomically, tryReadTChan, readTVar, writeTVar)

-- Application modules
import Message
import WebsocketApp (Clients)
import qualified STM.FileStore as STM (FileStore)
import qualified STM.Messages as STM (Messages, ServerMessages)
import ServerState
import qualified Handler.StorageHandler

data DispatchMessage = ServerMessage ServerMessage | Message Message | Empty

-- Dispatches messages from either a client or the server itself to the relevant message handler
-- Returns false if no messages are available to be processed
dispatch :: TVar ServerState -> Clients -> STM.FileStore -> STM.ServerMessages -> STM.Messages -> IO Bool
dispatch serverStateT clients fileStore serverMessages clientMessages = do
  -- If there are any messages in the server queue, process them first
  dispatchMessage <- atomically $ do
    serverMessage <- tryReadTChan serverMessages
    case serverMessage of
      Just sm -> return $ ServerMessage sm
      Nothing -> do
        -- If there are messages in the client queue, process them next
        clientMessage <- tryReadTChan clientMessages
        case clientMessage of
          Just cm -> return $ Message cm
          Nothing -> do
            serverState <- readTVar serverStateT
            if serverState == Terminating
              then (writeTVar serverStateT Terminated) >> return Empty 
              else return Empty
  -- Process the message, dispatching it to the relevant handler
  case dispatchMessage of
    ServerMessage message -> (processServerMessage clients fileStore message) >> return True
    Message message -> (processClientMessage clients fileStore message) >> return True
    Empty -> return False

processServerMessage :: Clients -> STM.FileStore -> ServerMessage -> IO ()
processServerMessage clients fileStore message =
  case message of
    -- Server messages
    ServerReloadWatchPath files -> reloadWatchPath files 
    ServerLoadFile storageEvent fileInfo -> loadFile storageEvent fileInfo
    ServerUnloadFile storageEvent fileInfo -> unloadFile storageEvent fileInfo
    -- Unknown message (error)
    _ -> undefined
  where
    reloadWatchPath = Handler.StorageHandler.reloadWatchPath clients fileStore
    loadFile = Handler.StorageHandler.loadFile clients fileStore
    unloadFile = Handler.StorageHandler.unloadFile clients fileStore

processClientMessage :: Clients -> STM.FileStore -> Message -> IO ()
processClientMessage clients fileStore message =
  case message of
    -- Unknown message (error)
    _ -> undefined
