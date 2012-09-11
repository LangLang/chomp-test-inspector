module STM.Messages (Messages, ServerMessages, newIO, enqueueMessage, enqueueServerMessage) where

-- Standard modules
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
--import Control.Monad (unless)

-- Application modules
import Message

type Messages = TChan Message
type ServerMessages = TChan ServerMessage

-- Create a new message queue
newIO :: IO (TChan a)
newIO = newTChanIO

-- Add a message to the queue as a single atomic transaction
enqueueMessage :: Messages -> Message -> IO ()
enqueueMessage m = atomically . (writeTChan m)

enqueueServerMessage :: ServerMessages -> ServerMessage -> IO ()
enqueueServerMessage m = atomically . (writeTChan m)

{-- Clear all the messages from the queue without processing them using multiple atomic operations 
clearMessages :: Messages -> IO ()
clearMessages m = do
  done <- atomically $ do 
    isEmpty <- isEmptyTChan m
    unless isEmpty $ readTChan m >> return ()
    return isEmpty
  if done
    then return ()
    else clearMessages m
--}