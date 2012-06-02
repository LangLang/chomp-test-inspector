module STM.Messages (Messages, newIO, enqueueMessage) where

-- Standard modules
import Control.Concurrent.STM (atomically, writeTChan)
import Control.Concurrent.STM.TChan

-- Application modules
import Message

type Messages = TChan Message

-- Create a new message queue
newIO :: IO Messages
newIO = newTChanIO

-- Add a message to the queue as a single atomic transaction
enqueueMessage :: Messages -> Message -> IO ()
enqueueMessage messages = atomically . (writeTChan messages)
