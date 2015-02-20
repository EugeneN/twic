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


hideMessage state msgid = do
    State { oldFeed     = of_
          , currentFeed = cf
          , newFeed     = nf
          , historyButtonDisabled = hbd
          , errors      = es } <- readState state

    writeState state $ State { oldFeed:     of_
                             , currentFeed: cf
                             , newFeed:     nf
                             , historyButtonDisabled: hbd
                             , errors:      filter (\(Error _ msgid') -> msgid' /= msgid) es }
    pure unit

errorItem state (Error msg msgid) =
  D.div {className: "message"} [
      D.span {} [D.rawText msg]
    , D.button { onClick: \e -> hideMessage state msgid } [D.rawText "x"]]

errorsList :: ComponentClass { state :: RefVal State } {}
errorsList = createClass spec { displayName = "Messages", render = renderFun } where
    renderFun this = do
        State { oldFeed     = _
              , newFeed     = _
              , currentFeed = _
              , errors      = es } <- readState this.props.state

        pure $ D.div {className: "messages"} $ (errorItem this.props.state) <$> es
