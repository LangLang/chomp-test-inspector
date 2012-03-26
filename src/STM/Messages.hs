module STM.Messages (Messages) where

-- Standard modules
import Control.Concurrent.STM.TChan

-- Application modules
import Message

type Messages = TChan Message
