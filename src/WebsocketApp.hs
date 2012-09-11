{-# LANGUAGE OverloadedStrings #-}
module WebsocketApp (Clients, websocketApp) where

-- Standard modules
import Prelude hiding (putStrLn)
import Network.WebSockets as WS hiding (Message, ParseError)
import Control.Exception (fromException)
import Data.Text (pack, unpack, append, Text)
import Data.Text.IO (putStrLn)
import Control.Monad (zipWithM_)
import Control.Monad.Trans (liftIO)
import Control.Exception (SomeException)
--import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (atomically, TVar, readTVarIO)
import Control.Concurrent.STM.TChan (writeTChan)
import qualified Data.STM.TList as TList
import Safe (readMay)

-- Application modules
import qualified FileStore
import FileStore (FileStore)
import qualified STM.Clients as STM (Clients)
import qualified STM.Clients as Clients
import Message
import qualified STM.Messages as STM (ServerMessages, Messages)
import ServerState

type Clients = STM.Clients Hybi10

-- Websocket application responsible for updating the client browser and receiving updates from the
-- the client
websocketApp :: TVar ServerState -> Clients -> FileStore -> STM.ServerMessages -> STM.Messages -> Request -> WebSockets Hybi10 ()
websocketApp serverStateT clients fileStore serverMessages clientMessages req = do
  WS.acceptRequest req
  liftIO $ putStrLn $ "Client connected (TODO: lookup client)..."

  -- Send the list of files and along with their cached contents & information (revision) to the client
  -- TODO: Read an "active" flag from the file store.
  --       If the file store is not active, then send ReloadFiles LostRootDirectory or similar instead
  files <- liftIO $ FileStore.allFilesIO fileStore
  maybeCacheEntries <- liftIO $  mapM (FileStore.readFileCacheEntryIO fileStore) files
  _ <- sendMessage $ ReloadFiles Connected files
  _ <- zipWithM_ sendLoadFileContents files maybeCacheEntries 
  
  -- Obtain a sink to use for sending data in another thread
  sink <- WS.getSink
  _ <- liftIO $ atomically $ TList.append clients ("Client",sink)
  
  -- Listen to incoming messages, adding them to a incomming client message queue
  -- TODO: listen should happen in a loop (probably forkIO'd and given the sink generated above)
  --       See STM.Clients
  listen serverStateT clients fileStore clientMessages
  
  -- TODO: Add the client information to a list
  -- TODO: forkIO $ processMessages serverMessages clientMessages
  where
    sendLoadFileContents :: FilePath -> Maybe FileStore.FileCacheEntry -> WebSockets Hybi10 ()
    sendLoadFileContents file maybeCacheEntry =
      sendMessage $ case maybeCacheEntry of
        Nothing -> UnloadFileContents file
        Just cacheEntry -> LoadFileContents file 
          (FileStore.revision (FileStore.cacheEntryInfo cacheEntry))
          (FileStore.cacheEntryContents cacheEntry)

sendMessage :: TextProtocol p => Message -> WebSockets p () 
sendMessage message = do
  liftIO $ putStrLn $ "...send " `append` (pack $ show message)
  sendTextData . pack $ show message

listen :: TVar ServerState -> Clients -> FileStore -> STM.Messages -> WS.WebSockets Hybi10 ()
listen serverStateT clients fileStore messages = do
  clientConnected <- (flip WS.catchWsError catchDisconnect) receiveMessage
  if not clientConnected
    then return ()
    else do 
      state <- liftIO $ readTVarIO serverStateT 
      if not $ state `elem` [Terminating, Terminated]
        then listen serverStateT clients fileStore messages
        else return ()
  where
    receiveMessage :: WS.WebSockets Hybi10 Bool
    receiveMessage = do
      messageString <- WS.receiveData :: WS.WebSockets Hybi10 Text
      liftIO $ do
        putStrLn $ "Message received from client (TODO: lookup client)..."
        putStrLn $ "...message content: '" `append` messageString `append` "'"
      case readMay $ unpack messageString :: Maybe Message of
        Just message ->
          liftIO $ atomically $ writeTChan messages $ message
          --sendMessage Acknowledge
        Nothing -> do
          liftIO $ putStrLn "...(error) could not parse recieved message"
          sendMessage $ ParseError $ take 255 $ unpack messageString
      return True

    catchDisconnect :: SomeException -> WebSockets p Bool
    catchDisconnect e =
      case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
          Clients.broadcastMessage clients $ Notify $ ClientDisconnected "TODO: client identifier"
          return False
        _ -> return False
