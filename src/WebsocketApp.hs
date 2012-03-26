{-# LANGUAGE OverloadedStrings #-}
module WebsocketApp (websocketApp) where

-- Standard modules
import Prelude hiding (putStrLn)
import Network.WebSockets as WS
import Data.Monoid (mappend)
import Control.Exception (fromException)
import Data.Text (pack, unpack, intercalate, append, Text)
import Data.Text.IO (putStrLn)
import Control.Monad.Trans (liftIO)
import Control.Exception (SomeException)
--import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import Data.STM.TList (TList)
import Data.Monoid
import qualified Data.STM.TList as TList

-- Application modules
import FileStore
import qualified STM.FileStore as STM (FileStore)
import qualified STM.Clients as STM (Clients)
import qualified STM.Clients as STM.Clients
import Message
import qualified STM.Messages as STM (Messages)

-- Websocket application responsible for updating the client browser and receiving updates from the
-- the client
websocketApp :: STM.FileStore -> Request -> WebSockets Hybi10 ()
websocketApp fileStore req = do
  WS.acceptRequest req

  -- Send the initial files to the application
  files <- liftIO $ atomically $ TList.toList fileStore
  sendTextData $ pack $ show $ ReloadFiles files

  -- Obtain a sink to use for sending data in another thread
  sink <- WS.getSink

  messages <- liftIO (newTChanIO :: IO STM.Messages)
  listen fileStore messages

listen :: STM.FileStore -> STM.Messages -> WS.WebSockets Hybi10 ()
listen fileStore messages = do
  clients <- liftIO $ TList.emptyIO
  (flip WS.catchWsError $ catchDisconnect clients) receive
  return ()
  {-
  liftIO $ readMVar state >>= broadcast
    (user `mappend` ": " `mappend` msg)
    talk state client -}
  where
    receive :: WS.WebSockets Hybi10 ()
    receive = do
      message <- WS.receiveData :: WS.WebSockets Hybi10 Text
      liftIO $ putStrLn ("Message received: '" `append` message `append` "'")
      liftIO $ atomically $ writeTChan messages $ read $ unpack message
      sendTextData $ pack $ show Acknowledge
      return ()
    catchDisconnect :: WS.TextProtocol p => STM.Clients p -> SomeException -> WebSockets p ()
    catchDisconnect clients e =
      case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
          STM.Clients.broadcast clients $ pack $ show $ Information "A client disconnected."
          return ()
        _ -> return ()
