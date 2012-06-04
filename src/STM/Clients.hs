{-# LANGUAGE OverloadedStrings #-}
module STM.Clients (Clients, newIO, broadcastMessage) where

-- Standard modules
import Prelude hiding (putStrLn)
import Control.Concurrent.STM (atomically)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList
import Data.Text (Text, pack, append)
import Data.Text.IO (putStrLn)
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS
import Control.Monad.Trans (liftIO)

-- Application modules
import Message

type Client p = (Text, WS.Sink p)
type Clients p = TList (Client p)

-- Create a new empty list of clients as a single IO operation
newIO :: WS.Protocol p => IO (Clients p)
newIO = TList.emptyIO

-- Broadcast a message to all the clients in the list
broadcastMessage :: WS.TextProtocol p => Clients p -> Message -> IO ()
broadcastMessage clients message = do
  putStrLn $ "Broadcast message...\n..." `append` serialize message
  broadcast clients $ serialize message

-- Send a message to a single client
sendMessage :: WS.TextProtocol p => Client p -> Message -> IO ()
sendMessage client message = do
  putStrLn $ "Send message to client " `append` fst client `append` "...\n..." `append` serialize message  
  send client $ serialize message

broadcast :: WS.TextProtocol p => Clients p -> Text -> IO ()
broadcast clients datum = do
  --forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message
  (atomically $ TList.toList clients) >>= (mapM_ $ (flip WS.sendSink $ WS.textData datum) . snd)
  --(atomically $ TList.toList clients) >>= (mapM_ $ putStrLn . fst)
  return ()

send :: WS.TextProtocol p => Client p -> Text -> IO ()
send client datum = do
  liftIO $ putStrLn datum
  WS.sendSink (snd client) $ WS.textData datum
  return ()

serialize :: Message -> Text
serialize = pack . show
