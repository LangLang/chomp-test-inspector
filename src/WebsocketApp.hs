module WebsocketApp (websocketApp) where

import Network.WebSockets
import Data.Text

websocketApp :: Request -> WebSockets Hybi10 ()
websocketApp req = sendTextData (pack "This is a test" :: Text)
