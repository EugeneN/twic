-- inherited from https://github.com/hdgarrood/multipac/blob/master/src/BrowserWebSocket.purs

module Lib.WebSocket where

import Data.Tuple
import Data.Function
import Data.Maybe
import Control.Monad.Eff
import Prelude

foreign import data WebSocket :: !
foreign import data Socket :: *
foreign import data CloseEvent :: *

-- for now, only UTF8 messages are supported
-- TODO: node buffers
type Message = String

foreign import mkWebSocket :: forall e. String -> Eff (ws :: WebSocket | e) Socket

foreign import onMessageImpl
  :: forall e a.
  Fn2
    Socket
    (Message -> Eff (ws :: WebSocket | e) a)
    (Eff (ws :: WebSocket | e) Unit)

onMessage :: forall e a.
  Socket
  -> (Message -> Eff (ws :: WebSocket | e) a)
  -> Eff (ws :: WebSocket | e) Unit
onMessage socket callback =
  runFn2 onMessageImpl socket callback


foreign import onErrorImpl
  :: forall e a.
  Fn2
    Socket
    (Eff (ws :: WebSocket | e) a)
    (Eff (ws :: WebSocket | e) Unit)

onError :: forall e a.
  Socket
  -> (Eff (ws :: WebSocket | e) a)
  -> (Eff (ws :: WebSocket | e) Unit)
onError socket callback =
  runFn2 onErrorImpl socket callback

foreign import onCloseImpl
  :: forall e a.
  Fn2
    Socket
    (Eff (ws :: WebSocket | e) a)
    (Eff (ws :: WebSocket | e) Unit)

onClose :: forall e a.
  Socket
  -> (Eff (ws :: WebSocket | e) a)
  -> (Eff (ws :: WebSocket | e) Unit)
onClose socket callback =
  runFn2 onCloseImpl socket callback

foreign import sendImpl :: forall e.
  Fn2 Socket Message (Eff (ws :: WebSocket | e) Unit)

send :: forall e.
  Socket -> Message -> Eff (ws :: WebSocket | e) Unit
send sock msg = runFn2 sendImpl sock msg
