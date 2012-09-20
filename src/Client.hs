module Client (Client(..), HostId, serverId) where

-- Standard modules
import Data.Text (Text)
import Control.Concurrent.STM (TVar)
import qualified Network.WebSockets as WS (Sink)

type HostId = Integer
data Client p = Client {
    clientId :: HostId,
    clientHost :: Text, 
    clientName :: TVar Text, 
    clientSink :: WS.Sink p
  }
  
-- Host identifier reserved for the server
serverId :: HostId
serverId = 0