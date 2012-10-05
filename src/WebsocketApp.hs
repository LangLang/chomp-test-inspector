{-# LANGUAGE OverloadedStrings #-}
module WebsocketApp (Clients, websocketIntercept, websocketApp, WebsocketAppState(..)) where

-- Standard modules
import Prelude hiding (putStrLn)
import Network.WebSockets as WS hiding (Message, ParseError)
import qualified Network.Wai.Handler.Warp as WaiWarp (Connection)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai as Wai
import Control.Exception (fromException)
import Data.Text (Text, pack, unpack, append, snoc)
import Data.Text.IO (putStrLn)
import qualified Data.Conduit as Conduit
import Data.ByteString (ByteString)
import Control.Monad (zipWithM_, (<=<))
import Control.Monad.Trans (liftIO)
import Control.Exception (SomeException)
--import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (atomically, TVar, readTVarIO, newTVarIO, readTVar, writeTVar)
import qualified Data.STM.TList as TList
import Safe (readMay)

-- Application modules
import qualified FileStore as FS
import FileStore (FileStore)
import qualified STM.Clients as STM
import Client (HostId, clientId)
import Message
import qualified STM.Messages as STM (ServerMessages, NetworkMessages)
import qualified STM.Messages
import ServerState

type Client = STM.Client Hybi10
type Clients = STM.Clients Hybi10
data WebsocketAppState = WebsocketAppState {
    appServerState :: TVar ServerState,
    appClients :: Clients, 
    appFileStore :: FileStore,
    appServerMessages :: STM.ServerMessages,  -- generated messages
    appClientMessages :: STM.NetworkMessages, -- received messages
    appClientIdCounter :: TVar HostId
  }
  
-- Increment the client id counter and return the previous value
generateClientId :: TVar HostId -> IO HostId
generateClientId counter = atomically $ do
  c <- (readTVar counter)
  writeTVar counter (c + 1)
  return c

-- This function is similar to interceptWith, but passes along additional information
-- (the client IP address) for logging purposes
websocketIntercept :: WebsocketAppState -> Wai.Request -> Maybe (Conduit.Source (Conduit.ResourceT IO) ByteString -> WaiWarp.Connection -> Conduit.ResourceT IO ())
websocketIntercept appState req = WaiWS.interceptWith defaultWebSocketsOptions (websocketApp appState req) req

-- Websocket application responsible for updating the client browser and receiving updates from the
-- the client
websocketApp :: WebsocketAppState -> Wai.Request -> WS.Request -> WebSockets Hybi10 ()
websocketApp appState waiReq wsReq = do
  WS.acceptRequest wsReq
  liftIO ((putStrLn $ "Client connected (" `append` showRemoteHost `append` ")...")
    >> (putStrLn $ "\t...incoming request: " `append` showWaiReq)
    >> (putStrLn $ "\t...incoming WebSocket request: " `append` (pack $ show wsReq))) 

  -- Send the list of files and along with their cached contents & information (revision) to the client
  -- TODO: Read an "active" flag from the file store.
  --       If the file store is not active, then send ReloadFiles LostRootDirectory or similar instead
  files <- liftIO $ FS.allFilesIO (appFileStore appState)
  maybeCacheEntries <- liftIO $ mapM (FS.readFileCacheEntryIO $ appFileStore appState) files
  _ <- sendMessage <=< (liftIO . stampServerMessage) $ ReloadFiles Connected files
  _ <- zipWithM_ sendLoadFileContents files maybeCacheEntries
  
  -- Obtain a sink to use for sending data in another thread
  newClientSink <- WS.getSink
  client <- liftIO $ do
    newClientName <- newTVarIO $ pack "Unnamed"
    newClientId <- generateClientId $ appClientIdCounter appState
    let c = STM.Client {
        STM.clientId = newClientId,
        STM.clientHost = showRemoteHost, 
        STM.clientName = newClientName,
        STM.clientSink = newClientSink
      }
    (atomically $ TList.append (appClients appState) c) >> return c 
  
  -- Listen to incoming messages, adding them to a incomming client message queue
  -- TODO: listen should happen in a loop (probably forkIO'd and given the sink generated above)
  --       See STM.Clients
  listen client (appServerState appState) (appClients appState) (appFileStore appState) (appClientMessages appState)
  
  -- TODO: Add the client information to a list
  -- TODO: forkIO $ processMessages serverMessages clientMessages
  where
    showRemoteHost ::Text
    showRemoteHost = (pack $ show $ Wai.remoteHost waiReq)
    
    showWaiReq :: Text
    showWaiReq = (pack $ show $ Wai.requestMethod waiReq) 
      `snoc` ' ' 
      `append` (pack $ show $ Wai.httpVersion waiReq)

    sendLoadFileContents :: FilePath -> Maybe FS.FileCacheEntry -> WebSockets Hybi10 ()
    sendLoadFileContents file maybeCacheEntry =
      sendMessage <=< (liftIO . stampServerMessage) $ case maybeCacheEntry of
        Nothing -> UnloadFileContents file
        Just cacheEntry -> LoadFileContents file 
          (FS.opsRevision $ FS.cacheEntryInfo cacheEntry)
          (FS.cacheEntryContents cacheEntry)

sendMessage :: TextProtocol p => StampedNetworkMessage -> WebSockets p () 
sendMessage message = do
  liftIO $ putStrLn $ "\t...send " `append` (pack $ show message)
  sendTextData . pack $ show message

listen :: Client -> TVar ServerState -> Clients -> FileStore -> STM.NetworkMessages -> WS.WebSockets Hybi10 ()
listen client serverStateT clients fileStore messages = do
  clientConnected <- (flip WS.catchWsError catchDisconnect) receiveMessage
  if not clientConnected
    then return ()
    else do 
      state <- liftIO $ readTVarIO serverStateT 
      if not $ state `elem` [Terminating, Terminated]
        then listen client serverStateT clients fileStore messages
        else return ()
  where
    receiveMessage :: WS.WebSockets Hybi10 Bool
    receiveMessage = do
      messageString <- WS.receiveData :: WS.WebSockets Hybi10 Text
      liftIO $ do
        clientSummary <- STM.showClientSummaryIO client  
        putStrLn $ "Message received from client " `append` clientSummary `append` "..."
      case readMay $ unpack messageString :: Maybe NetworkMessage of
        Just message ->
          liftIO $ (STM.Messages.enqueue messages <=< stampClientMessage (clientId client)) message
          --sendMessage Acknowledge
        Nothing -> do
          liftIO $ putStrLn "\t...(error) could not parse recieved message"
          sendMessage <=< (liftIO . stampServerMessage) $ ParseError $ take 255 $ unpack messageString
      return True

    catchDisconnect :: SomeException -> WebSockets p Bool
    catchDisconnect e =
      case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
          (STM.broadcastMessage clients <=< stampServerMessage) $ Notify $ ClientDisconnected "TODO: client identifier"
          return False
        _ -> return False
