{-# LANGUAGE OverloadedStrings #-}
module WebsocketApp (websocketApp) where

-- Standard modules
import Prelude hiding (putStrLn)
import Network.WebSockets as WS hiding (Message, ParseError)
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
import Safe (readMay)

-- Application modules
import FileStore
import qualified STM.FileStore as STM (FileStore)
import qualified STM.Clients as STM (Clients)
import qualified STM.Clients as STM.Clients
import Message
import qualified STM.Messages (newIO)
import qualified STM.Messages as STM (Messages)

-- Websocket application responsible for updating the client browser and receiving updates from the
-- the client
websocketApp :: STM.FileStore -> Request -> WebSockets Hybi10 ()
websocketApp fileStore req = do
  WS.acceptRequest req
  liftIO $ putStrLn $ "Client connected (TODO: lookup client)..."

  -- Send the initial files to the client application
  files <- liftIO $ atomically $ TList.toList fileStore
  sendMessage $ ReloadFiles files

  -- Obtain a sink to use for sending data in another thread
  sink <- WS.getSink

  messages <- liftIO STM.Messages.newIO
  
  -- TODO: listen should happen in a loop (probably forkIO'd and given the sink generated above)
  --       See STM.Clients
  listen fileStore messages
  
  -- TODO: forkIO $ processMessages serverMessages clientMessages
  -- TODO: pass server messages into this function  

sendMessage :: TextProtocol p => Message -> WebSockets p () 
sendMessage message = do
  liftIO $ putStrLn $ "...send " `append` (pack $ show message)
  sendTextData . pack $ show message

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
      messageString <- WS.receiveData :: WS.WebSockets Hybi10 Text
      liftIO $ do
        putStrLn $ "Message received from client (TODO: lookup client)..."
        putStrLn $ "...message content: '" `append` messageString `append` "'"
      case readMay $ unpack messageString :: Maybe Message of
        Just message -> do
          liftIO $ atomically $ writeTChan messages $ message
          sendMessage Acknowledge
        Nothing -> do
          liftIO $ putStrLn "...(error) could not parse recieved message"
          sendMessage $ ParseError $ take 255 $ unpack messageString
      return ()

    catchDisconnect :: WS.TextProtocol p => STM.Clients p -> SomeException -> WebSockets p ()
    catchDisconnect clients e =
      case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
          STM.Clients.broadcast clients $ pack $ show $ Notify $ ClientDisconnected "TODO: client identifier"
          return ()
        _ -> return ()
