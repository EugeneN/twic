module UI.Messages where

import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec)
import React.Types (Component(), ComponentClass(),  React())


messageComponent :: ComponentClass {message :: String} {}
messageComponent = createClass spec { displayName = "Message", render = renderFun } where
    renderFun this = pure $ D.div {className: "message"} [D.rawText this.props.message]

renderMessage :: forall eff. String -> String -> Eff (dom :: DOM, react :: React | eff) Component
renderMessage targetId m = renderComponentById (messageComponent {message: m} []) targetId
