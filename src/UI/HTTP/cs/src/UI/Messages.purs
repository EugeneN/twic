module UI.Messages where


import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec)
import React.Types (Component(), ComponentClass(),  React())
import Types
import Data.Array



errorItem (Error msg) = D.div {className: "message"} [D.rawText msg]

errorsList :: ComponentClass {messages :: [Error]} {}
errorsList = createClass spec { displayName = "Messages", render = renderFun } where
    renderFun this = pure $ D.div {className: "messages"} $ errorItem <$> this.props.messages

renderMessage :: forall eff. String -> [Error] -> Eff (dom :: DOM, react :: React | eff) Component
renderMessage targetId es = renderComponentById (errorsList {messages: es} []) targetId
