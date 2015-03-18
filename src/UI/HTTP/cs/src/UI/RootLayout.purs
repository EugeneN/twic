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
import UI.TweetWriter (writeInputComponent, hideWriteInput)
import UI.FeedMetadata (feedMetadata)
import UI.UserInfo (userInfo, hideUserInfo)
import UI.MyInfo (myInfo)
import UI.SearchInput (searchInputComponent)
import Types
import Core
import Utils
import Data.Maybe

contextMenuHandler state ev = do
    stopPropagation ev
    resetContextMenu state


resetMenus state = do
  resetContextMenu state
  hideUserInfo state
  hideWriteInput state

rootLayout :: ComponentClass { state :: RefVal State } {}
rootLayout =
    createClass spec { displayName = "RootLayout", render = renderFun }
    where
    renderFun this = do
      State { feed = AFeed {oldFeed     = (OldFeed of_)
                           , newFeed     = (NewFeed nf)
                           , currentFeed = (CurrentFeed cf) }
            , errors      = es } <- readState this.props.state

      pure $
        D.div { className: "root-layout"
              , onContextMenu: (callEventHandler $ contextMenuHandler this.props.state)
              , onClick: (resetMenus this.props.state) } [
            D.div { className:  "error" , id: "messages" } [
              (errorsList {state: this.props.state} [])]

          , D.div { id: "feed-metadata-container-id" } [
              (feedMetadata {state: this.props.state} [])]

          , D.div { id: "load-history-container-id" } [
              (historyButton {state: this.props.state} [])]

          , D.div { className:  "container" , id: "container" } [
              (tweetsList {state: this.props.state} [])]

          , D.div { className:  "refresh" , id: "refresh" } [
              (checkButton {state: this.props.state} []) ]

          , D.div { id: "write-tweet-container-id" } [
              (writeInputComponent {state: this.props.state} [])]

          , D.div { id: "ctx-menu-container-id" } [
              (tweetMenu {state: this.props.state} [])]

          , D.div { id: "userinfo-container-id" } [
              (userInfo {state: this.props.state} [])]

          , D.div { id: "myinfo-container-id" } [
              (myInfo {state: this.props.state} [])]

          , D.div { id: "searchinput-container-id" } [
              (searchInputComponent {state: this.props.state} [])]
        ]

renderRootLayout :: forall eff. String
                             -> RefVal State
                             -> Eff (dom :: DOM, react :: React | eff) Component
renderRootLayout targetId state =
    renderComponentById (rootLayout { state: state } []) targetId


