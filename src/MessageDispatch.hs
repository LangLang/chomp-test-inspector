module MessageDispatch (dispatch) where

-- TODO: Should this be called Scheduler?

-- Standard modules
import Control.Concurrent.STM (TVar, atomically, tryReadTChan, readTVar, writeTVar)

-- Application modules
import Message
import WebsocketApp (Clients)
import FileStore (FileStore)
import qualified STM.Messages as STM (Messages, ServerMessages)
import ServerState
import qualified Handler.StorageHandler

data DispatchMessage = ServerMessage ServerMessage | Message Message | Empty

-- Dispatches messages from either a client or the server itself to the relevant message handler
-- Returns false if no messages are available to be processed
dispatch :: TVar ServerState -> Clients -> FileStore -> STM.ServerMessages -> STM.Messages -> Maybe FilePath -> IO Bool
dispatch serverStateT clients fileStore serverMessages clientMessages maybeExecPath = do
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
    ServerMessage message -> (processServerMessage fileStore serverMessages clients maybeExecPath message) >> return True
    Message message -> (processClientMessage fileStore serverMessages clients message) >> return True
    Empty -> return False

processServerMessage :: FileStore -> STM.ServerMessages -> Clients -> Maybe FilePath -> ServerMessage -> IO ()
processServerMessage fileStore serverMessages clients maybeExecPath message =
  Handler.StorageHandler.handler fileStore serverMessages clients maybeExecPath message

processClientMessage :: FileStore -> STM.ServerMessages -> Clients -> Message -> IO ()
processClientMessage fileStore serverMessages clients message =
  case message of
    -- Unknown message (error)
    _ -> undefined
