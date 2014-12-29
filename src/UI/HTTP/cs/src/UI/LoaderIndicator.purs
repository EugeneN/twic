module UI.LoaderIndicator where

import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec)
import React.Types (Component(), ComponentClass(),  React())


showLoaderComponent :: ComponentClass {} {}
showLoaderComponent = createClass spec { displayName = "Loader", render = renderFun } where
                      renderFun this = pure $ D.div {className: "no-tweets"} [
                                                D.img {src: "http://eugenen.github.io/resources/public/img/loading1.gif"} []]

hideLoaderComponent :: ComponentClass {} {}
hideLoaderComponent = createClass spec { displayName = "Loader", render = renderFun } where
                      renderFun this = pure $ D.div {className: "no-tweets"} []


showLoader :: forall eff. String -> Eff (dom :: DOM, react :: React | eff) Component
showLoader targetId = renderComponentById (showLoaderComponent {} []) targetId

hideLoader :: forall eff. String -> Eff (dom :: DOM, react :: React | eff) Component
hideLoader targetId = renderComponentById (hideLoaderComponent {} []) targetId
