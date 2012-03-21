{-# LANGUAGE OverloadedStrings #-}
module WebsocketApp (websocketApp) where

-- Standard modules
import Network.WebSockets as WS
import Data.Text (pack, intercalate, Text)
import Control.Monad.Trans (liftIO)
--import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (atomically)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList

-- Application modules
import FileStore
import qualified STM.FileStore as STM (FileStore)

data Message = ReloadFiles [FileInfo]
  deriving Show

-- Websocket application responsible for updating the client browser and receiving updates from the
-- the client
websocketApp :: STM.FileStore -> Request -> WebSockets Hybi10 ()
websocketApp fileStore req = do
  WS.acceptRequest req
  -- Obtain a sink to use for sending data in another thread
  --sink <- WS.getSink
  --msg <- WS.receiveData
  --liftIO $ WS.sendSink sink $ WS.textData "Test message"

  -- Send the initial files to the application
  sendTextData ("TODO: Send files" :: Text)
  files <- liftIO $ atomically $ TList.toList fileStore
  --sendTextData $ intercalate "\n" . map pack $ files
  sendTextData $ pack $ show $ ReloadFiles files
