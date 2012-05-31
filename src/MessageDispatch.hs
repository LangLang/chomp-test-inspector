module MessageDispatch (dispatch) where

-- Standard modules
import Data.Maybe (isJust)
import Control.Concurrent.STM (TVar, atomically, tryReadTChan, readTVar, writeTVar)

-- Application modules
import Message
import WebsocketApp (Clients)
import qualified STM.FileStore as STM (FileStore)
import qualified STM.FileStore as STM.FileStore
import qualified STM.Messages as STM (Messages)
import ServerState

processMessage :: Clients -> STM.FileStore -> STM.Messages -> Maybe Message -> IO Bool
processMessage clients fileStore messages maybeMessage = return False

-- Dispatches messages from either a client or the server itself to the relevant message handler
-- Returns false if no messages are available to be processed
dispatch :: TVar ServerState -> Clients -> STM.FileStore -> STM.Messages -> STM.Messages -> IO Bool
dispatch serverStateT clients fileStore serverMessages clientMessages = do
  -- If there are any messages in the server queue, process them first
  message <- atomically $ do
    message <- tryReadTChan serverMessages
    if isJust message
      then return message
      else do
        -- If there are messages in the client queue, process them next
        message <- tryReadTChan clientMessages
        if isJust message
          then return message
          else do
            serverState <- readTVar serverStateT
            if serverState == Terminating
              then (writeTVar serverStateT Terminated) >> return Nothing 
              else return Nothing
  -- TODO... 
  if isJust message   
    then return True
    else return False
  where
    messageHandler = processMessage clients fileStore