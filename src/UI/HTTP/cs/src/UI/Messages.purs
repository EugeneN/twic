module UI.Messages where


import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import React hiding (readState, writeState)
import qualified React.DOM as D
import qualified React.DOM.Props as P
import Types
import Core (readState, writeState)
import Data.Array
import Control.Monad.Eff.Ref
import Utils
import Optic.Setter ((.~))
import Optic.Getter ((^.))
import Core
import Prelude


hideMessage :: forall eff. Ref State -> Msgid -> Eff (ref :: REF | eff) Unit
hideMessage state msgid = do
    s <- readState state
    writeState state (s # messagesL .~ (filter filterOutMsg (s ^. messagesL)))

    where
    filterOutMsg (Error _ msgid')   = msgid' /= msgid
    filterOutMsg (Success _ msgid') = msgid' /= msgid
    filterOutMsg (Other _ msgid')   = msgid' /= msgid

messageItemBody state msg msgid colorClass =
  D.div [P.className $ "message " ++ colorClass] [
      D.span [] [D.text msg]
    , D.button [ P.className "remove-message"
               , P.onClick hideMessage state msgid ] [D.text "x"]]

messageItem state (Error msg msgid)   = messageItemBody state msg msgid "red"
messageItem state (Success msg msgid) = messageItemBody state msg msgid "green"
messageItem state (Other msg msgid)   = messageItemBody state msg msgid "blue"

-- TODO rename errorsList to messagesList
errorsList = mkUI $ spec {} \this -> do
        state@(State s) <- readState this.props.state
        p@{state = (State s)} <- getProps this
        pure $ D.div [ P.className "messages"] $ (messageItem state) <$> s.errors
