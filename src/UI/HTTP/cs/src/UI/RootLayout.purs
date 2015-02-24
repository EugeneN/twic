module UI.RootLayout where

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Ref
import DOM (DOM(..))
import qualified React.DOM as D
import React ( createClass , eventHandler , renderComponentById , spec )
import React.Types ( Component() , ComponentClass() , Event() , React()
                   , ReactFormEvent() , ReactThis() )

import UI.Feed (tweetsList, checkButton, historyButton, tweetMenu)
import UI.Messages (errorsList)
import Types
import Core
import Utils
import Data.Maybe

resetContextMenu state e = do
    State { oldFeed =     of_
          , currentFeed = cf
          , newFeed =     nf
          , historyButtonDisabled = hbd
          , contextMenu = _
          , errors =      es } <- readState state

    writeState state $ State { oldFeed:     of_
                             , currentFeed: cf
                             , newFeed:     nf
                             , historyButtonDisabled: hbd
                             , contextMenu: ContextMenu { visible: false
                                                        , x: 0
                                                        , y: 0
                                                        , tweetId: Nothing }
                             , errors:      es }
    pure unit

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
        D.div { className: "root-layout"
              , onClick: (callEventHandler $ resetContextMenu this.props.state) } [
            D.div { className:  "error" , id: "messages" } [
              (errorsList {state: this.props.state} [])]

          , D.div { id: "load-history-container-id" } [
              (historyButton {state: this.props.state} [])]

          , D.div { className:  "container" , id: "container" }
              [(tweetsList {state: this.props.state} [])]

          , D.div { className:  "refresh" , id: "refresh" } [
              (checkButton {state: this.props.state} []) ]

          , D.div { id: "write-tweet-container-id" } []
          , D.div { id: "ctx-menu-container-id" } [
              (tweetMenu {state: this.props.state} [])]
        ]

renderRootLayout :: forall eff. String
                             -> RefVal State
                             -> Eff (dom :: DOM, react :: React | eff) Component
renderRootLayout targetId state =
    renderComponentById (rootLayout { state: state } []) targetId


