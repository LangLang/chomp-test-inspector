{-# LANGUAGE OverloadedStrings #-}

module WebsocketApp (websocketApp) where

import Network.WebSockets as WS
import Data.Text (pack, Text)
import Control.Monad.Trans (liftIO)

websocketApp :: Request -> WebSockets Hybi10 ()
websocketApp req = do
  WS.acceptRequest req
  -- Obtain a sink to use for sending data in another thread
  --sink <- WS.getSink
  --msg <- WS.receiveData
  --liftIO $ WS.sendSink sink $ WS.textData "Test message"
  sendTextData ("This is a test" :: Text)
