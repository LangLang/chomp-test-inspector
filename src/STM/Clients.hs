{-# LANGUAGE OverloadedStrings #-}
module STM.Clients (Client(..), Clients, showClientSummaryIO, newIO, broadcastMessage) where

-- Standard modules
import Prelude hiding (putStrLn)
import Control.Concurrent.STM (atomically, TVar, readTVarIO)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList
import Data.Text (Text, pack, append, snoc)
import Data.Text.IO (putStrLn)
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS
import Control.Monad.Trans (liftIO)

-- Application modules
import Message

data Client p = Client {
    clientId :: Integer,
    clientHost :: Text, 
    clientName :: TVar Text, 
    clientSink :: WS.Sink p
  }
type Clients p = TList (Client p)

-- Atomically read the client name 
clientNameIO :: Client p -> IO Text
clientNameIO = readTVarIO . clientName

-- Serialize a summary of the client information for log messages
showClientSummaryIO :: Client p -> IO Text
showClientSummaryIO client = do
  name <- clientNameIO client 
  return $ name 
    `append` (pack " (") 
    `append` (clientHost client)
    `snoc` ')'

-- Create a new empty list of clients as a single IO operation
newIO :: WS.Protocol p => IO (Clients p)
newIO = TList.emptyIO

-- Broadcast a message to all the clients in the list
broadcastMessage :: WS.TextProtocol p => Clients p -> Message -> IO ()
broadcastMessage clients message = do
  putStrLn $ "Broadcast message...\n\t..." `append` showSummary message
  broadcast clients $ serialize message

-- Send a message to a single client
sendMessage :: WS.TextProtocol p => Client p -> Message -> IO ()
sendMessage client message = do
  name <- clientNameIO $ client
  (putStrLn $ "Send message to client " 
    `append` name  
    `append` " (" 
    `append` (clientHost client)
    `append` "...\n\t..."
    `append` showSummary message)  
  >> (send client $ serialize message)

broadcast :: WS.TextProtocol p => Clients p -> Text -> IO ()
broadcast clients datum = do
  --forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message
  (atomically $ TList.toList clients) >>= (mapM_ $ (flip WS.sendSink $ WS.textData datum) . clientSink)
  --(atomically $ TList.toList clients) >>= (mapM_ $ putStrLn . fst)
  return ()

send :: WS.TextProtocol p => Client p -> Text -> IO ()
send client datum = do
  liftIO $ putStrLn datum
  WS.sendSink (clientSink client) $ WS.textData datum
  return ()

serialize :: Message -> Text
serialize = pack . show
