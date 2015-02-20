module UI.RootLayout where

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Ref
import DOM (DOM(..))
import qualified React.DOM as D
import React ( createClass , eventHandler , renderComponentById , spec )
import React.Types ( Component() , ComponentClass() , Event() , React()
                   , ReactFormEvent() , ReactThis() )

import UI.Feed (tweetsList, checkButton, historyButton)
import UI.Messages (errorsList)
import Types
import Core


rootLayout :: ComponentClass { state :: RefVal State } {}
rootLayout =
    createClass spec { displayName = "RootLayout", render = renderFun }
    where
    renderFun this = do
      State { oldFeed     = (OldFeed of_)
            , newFeed     = (NewFeed nf)
            , currentFeed = (CurrentFeed cf)
            , errors      = es } <- readState this.props.state

      pure $
        D.div { className: "root-layout" } [
            D.div { className:  "error" , id: "messages" } [
              (errorsList {state: this.props.state} [])]

          , D.div { id: "load-history-container-id" } [
              (historyButton {state: this.props.state} [])]

          , D.div { className:  "container" , id: "container" }
              [(tweetsList {state: this.props.state} [])]

          , D.div { id: "write-tweet-container-id" } []
          , D.div { className:  "refresh" , id: "refresh" } [
              (checkButton {state: this.props.state} []) ]]

renderRootLayout :: forall eff. String
                             -> RefVal State
                             -> Eff (dom :: DOM, react :: React | eff) Component
renderRootLayout targetId state =
    renderComponentById (rootLayout {state: state} []) targetId


