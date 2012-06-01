module STM.Clients (Clients, newIO, broadcastMessage) where

-- Standard modules
import Prelude hiding (putStrLn)
import Control.Concurrent.STM (atomically)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS
import Control.Monad.Trans (liftIO)

-- Application modules
import Message

type Client p = (Text, WS.Sink p)
type Clients p = TList (Client p)

newIO :: WS.Protocol p => IO (Clients p)
newIO = TList.emptyIO

-- Broadcast a message to all the clients in the list
broadcastMessage :: WS.TextProtocol p => Clients p -> Message -> IO ()
broadcastMessage clients message = broadcast clients $ pack $ show message

-- Send a message to a single client
--sendMessage :: WS.TextProtocol p => Client p -> Message -> IO ()
--sendMessage clients message = broadcast client $ show message

broadcast :: WS.TextProtocol p => Clients p -> Text -> IO ()
broadcast clients message = do
  liftIO $ putStrLn message
  --forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message
  (atomically $ TList.toList clients) >>= (mapM_ $ (flip WS.sendSink $ WS.textData message) . snd)
  return ()
