module UI.LoaderIndicator where

import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import React
import qualified React.DOM as D
import qualified React.DOM.Props as P
import Prelude

showLoaderComponent = mkUI $ spec {} \this ->
    pure $ D.div [P.className "no-tweets"] [
        D.img [P.src "http://eugenen.github.io/resources/public/img/loading1.gif"] []]

hideLoaderComponent = mkUI $ spec {} \this -> pure $ D.div [P.className "no-tweets"] []

showLoader :: forall eff. String -> Eff (dom :: DOM | eff) UI
showLoader targetId = renderToElementById targetId (showLoaderComponent {})

hideLoader :: forall eff. String -> Eff (dom :: DOM | eff) UI
hideLoader targetId = renderToElementById targetId (hideLoaderComponent {})
