module STM.Messages (Messages, newIO) where

-- Standard modules
import Control.Concurrent.STM.TChan

-- Application modules
import Message

type Messages = TChan Message

newIO :: IO Messages
newIO = newTChanIO
