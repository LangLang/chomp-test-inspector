module STM.Clients (Clients, newIO, broadcast) where

import Prelude hiding (putStrLn)
import Control.Concurrent.STM (atomically)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList
import Data.Text (Text)
import Data.Text.IO (putStrLn)
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
import qualified Network.WebSockets as WS
import Control.Monad.Trans (liftIO)

type Client p = (Text, WS.Sink p)
type Clients p = TList (Client p)

newIO :: WS.Protocol p => IO (Clients p)
newIO = TList.emptyIO

broadcast :: WS.TextProtocol p => Clients p -> Text -> IO ()
broadcast clients message = do
  liftIO $ putStrLn message
  --forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message
  (atomically $ TList.toList clients) >>= (mapM_ $ (flip WS.sendSink $ WS.textData message) . snd)
  return ()
