module STM.Messages (NetworkMessages, ServerMessages, newIO, enqueue) where

-- Standard modules
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
--import Control.Monad (unless)

-- Application modules
import Message

type NetworkMessages = TChan StampedNetworkMessage
type ServerMessages = TChan StampedServerMessage

-- Create a new message queue
newIO :: IO (TChan a)
newIO = newTChanIO

-- Add a message to the queue as a single atomic transaction
enqueue :: TChan m -> m -> IO ()
enqueue m = atomically . (writeTChan m)

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