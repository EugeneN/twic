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

rootLayout :: ComponentClass { state :: REFVal State } {}
rootLayout =
    mkUI $ spec {} 	his -> do
    where
    renderFun this = do
      State { feed = AFeed {oldFeed     = (OldFeed of_)
                           , newFeed     = (NewFeed nf)
                           , currentFeed = (CurrentFeed cf) }
            , errors      = es } <- readState this.props.state

      pure $
        D.div [ P.className "root-layout"
              , P.onDoubleClick (callEventHandler $ contextMenuHandler this.props.state)
              , P.onClick (resetMenus this.props.state) ] [
            D.div [ P.className  "error"
                  , P.id "messages" ] [
              (errorsList {state: this.props.state} [])]

          , D.div [ P.id "feed-metadata-container-id"] [
              (feedMetadata {state: this.props.state} [])]

          , D.div [ P.id "load-history-container-id"] [
              (historyButton {state: this.props.state} [])]

          , D.div [ P.className  "container" , id: "container"] [
              (tweetsList {state: this.props.state} [])]

          , D.div [ P.className  "refresh" , id: "refresh"] [
              (checkButton {state: this.props.state} []) ]

          , D.div [ P.id "write-tweet-container-id"] [
              (writeInputComponent {state: this.props.state} [])]

          , D.div [ P.id "ctx-menu-container-id"] [
              (tweetMenu {state: this.props.state} [])]

          , D.div [ P.id "userinfo-container-id"] [
              (userInfo {state: this.props.state} [])]

          , D.div [ P.id "myinfo-container-id"] [
              (myInfo {state: this.props.state} [])]

          , D.div [ P.id "searchinput-container-id"] [
              (searchInputComponent {state: this.props.state} [])]
        ]

renderRootLayout :: forall eff. String
                             -> Ref State
                             -> Eff (dom :: DOM, react :: React | eff) Component
renderRootLayout targetId state =
    renderComponentById (rootLayout { state: state } []) targetId
