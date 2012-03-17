{-# LANGUAGE OverloadedStrings #-}
module WebsocketApp (websocketApp) where

-- Standard modules
import Network.WebSockets as WS
import Data.Text (pack, Text)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)

-- Application modules
import FileStore

-- Websocket application responsible for updating the client browser and receiving updates from the
-- the client
websocketApp :: MVar FileStore -> Request -> WebSockets Hybi10 ()
websocketApp fileStore req = do
  WS.acceptRequest req
  -- Obtain a sink to use for sending data in another thread
  --sink <- WS.getSink
  --msg <- WS.receiveData
  --liftIO $ WS.sendSink sink $ WS.textData "Test message"

  -- Send the initial files to the application
  sendTextData ("TODO: Send files" :: Text)

