module UI.Messages where


import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec)
import React.Types (Component(), ComponentClass(),  React())
import Types
import Core (readState, writeState)
import Data.Array
import Control.Monad.Eff.Ref
import Utils
import Optic.Core ( (.~), (^.))
import Core


hideMessage :: forall eff. RefVal State -> Msgid -> Eff (ref :: Ref | eff) Unit
hideMessage state msgid = do
    s <- readState state
    writeState state (s # messagesL .~ (filter filterOutMsg (s ^. messagesL)))

    where
    filterOutMsg (Error _ msgid')   = msgid' /= msgid
    filterOutMsg (Success _ msgid') = msgid' /= msgid
    filterOutMsg (Other _ msgid')   = msgid' /= msgid

messageItemBody state msg msgid colorClass =
  D.div {className: "message " ++ colorClass} [
      D.span {} [D.rawText msg]
    , D.button { className: "remove-message"
               , onClick: hideMessage state msgid } [D.rawText "x"]]
               
messageItem state (Error msg msgid)   = messageItemBody state msg msgid "red"
messageItem state (Success msg msgid) = messageItemBody state msg msgid "green"
messageItem state (Other msg msgid)   = messageItemBody state msg msgid "blue"

-- TODO rename errorsList to messagesList
errorsList :: ComponentClass { state :: RefVal State } {}
errorsList = createClass spec { displayName = "Messages", render = renderFun } where
    renderFun this = do
        State { errors = es } <- readState this.props.state

        pure $ D.div {className: "messages"} $ (messageItem this.props.state) <$> es
