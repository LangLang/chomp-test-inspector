module MessageDispatch (dispatch) where

-- TODO: Should this be called Scheduler?

-- Standard modules
import Data.Maybe (isJust)
import Control.Concurrent.STM (TVar, atomically, tryReadTChan, readTVar, writeTVar)

-- Application modules
import Message
import WebsocketApp (Clients)
import qualified STM.FileStore as STM (FileStore)
import qualified STM.Messages as STM (Messages)
import ServerState
import qualified Handler.StorageHandler

-- Dispatches messages from either a client or the server itself to the relevant message handler
-- Returns false if no messages are available to be processed
dispatch :: TVar ServerState -> Clients -> STM.FileStore -> STM.Messages -> STM.Messages -> IO Bool
dispatch serverStateT clients fileStore serverMessages clientMessages = do
  -- If there are any messages in the server queue, process them first
  maybeMessage <- atomically $ do
    serverMessage <- tryReadTChan serverMessages
    if isJust serverMessage
      then return serverMessage
      else do
        -- If there are messages in the client queue, process them next
        clientMessage <- tryReadTChan clientMessages
        if isJust clientMessage
          then return clientMessage
          else do
            serverState <- readTVar serverStateT
            if serverState == Terminating
              then (writeTVar serverStateT Terminated) >> return Nothing 
              else return Nothing
  -- Process the message, dispatching it to the relevant handler
  case maybeMessage of
    Just message -> (processMessage clients fileStore clientMessages message) >> return True
    Nothing -> return False

processMessage :: Clients -> STM.FileStore -> STM.Messages -> Message -> IO ()
processMessage clients fileStore clientMessages message =
  case message of
    -- Server messages
    ReloadWatchPath -> reloadWatchPath clientMessages
    LoadFile _ _ -> loadFile message
    -- Client messages 
    
    -- Unknown message (error)
    _ -> undefined
  where
    reloadWatchPath = Handler.StorageHandler.reloadWatchPath clients fileStore
    loadFile = Handler.StorageHandler.loadFile clients fileStore