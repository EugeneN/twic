module UI.SearchInput where

import Debug.Trace
import Data.Maybe
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Ref
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec, eventHandler)
import React.Types (Component(), ComponentClass(),  React())

import qualified Rx.Observable as Rx
import Rx.JQuery
import qualified Control.Monad.JQuery as J

import Config (writeTweetContainerId, messagesId)
import Utils
import Types
import Core
import Optic.Core ( (.~), (^.))
import Data.Foldable
import Data.Array
import Data.String (indexOf)

showSearchInput :: forall eff. RefVal State
                            -> Eff (ref :: Ref, dom :: DOM, react :: React, trace :: Trace | eff) Unit
showSearchInput state = do
    s <- readState state
    writeState state (s # searchInputL .~ SearchInput { visible: true -- resetting all fields
                                                      , value: "" })
    forkPostpone (setFocus "search-input-id") 50

hideSearchInput :: forall eff. RefVal State
                            -> Eff (ref :: Ref, dom :: DOM, react :: React, trace :: Trace | eff) Unit
hideSearchInput state = do
    s <- readState state
    writeState state (s # searchInputL .~ ( (s ^. searchInputL) # searchInputVisibleL .~ false ) )

asText :: TweetElement -> String
asText (AtUsername s) = s
asText (Link s)       = s
asText (PlainText s)  = s
asText (Hashtag s)    = s
asText (Retweet s)    = ""
asText (Spaces s)     = s
asText (Unparsable s) = s

tweetElementMatch :: String -> TweetElement -> Boolean
tweetElementMatch st (AtUsername s) = strMatch st s
tweetElementMatch st (Link s)       = strMatch st s
tweetElementMatch st (PlainText s)  = strMatch st s
tweetElementMatch st (Hashtag s)    = strMatch st s
tweetElementMatch st (Retweet s)    = false
tweetElementMatch st (Spaces s)     = false
tweetElementMatch st (Unparsable s) = strMatch st s

strMatch :: String -> String -> Boolean
strMatch searchTerm text = indexOf searchTerm text >= 0

-- TODO better search algo w/ split by words, in usernames, via twitter api etc
searchFeed state = do
    s <- readState state
    let searchTerm = s ^. searchInputL ^. searchInputValueL
        feed = s ^. feedL
        result = filterFeeds searchTerm feed

    writeState state (s # extraFeedL .~ (Just $ BFeed { oldFeed     : OldFeed []
                                                      , currentFeed : CurrentFeed result
                                                      , newFeed     : NewFeed []
                                                      , author      : Nothing }))

    where
    filterFeeds searchTerm (AFeed { oldFeed = (OldFeed old)
                                  , currentFeed = (CurrentFeed cur)
                                  , newFeed = (NewFeed new) }) =
           (filterFeed searchTerm old)
        ++ (filterFeed searchTerm cur)
        ++ (filterFeed searchTerm new)

    filterFeed :: String -> [Tweet] -> [Tweet]
    filterFeed st fd = filter (searchWholeTweet st) fd

    searchWholeTweet :: String -> Tweet -> Boolean
    searchWholeTweet searchTerm (Tweet {text=body}) =
        strMatch searchTerm (intercalate "" (asText <$> body))

    searchTweet :: String -> Tweet -> Boolean
    searchTweet st (Tweet {text=body}) = any ((==) true) $ (tweetElementMatch st) <$> body

handleChange this k = do
    s <- readState this.props.state
    writeState this.props.state (s # searchInputL .~ ( (s ^. searchInputL) # searchInputValueL .~ (value k.target) ) )

    -- TODO make a [rx]stream and write to it, read throttled by time then perform search
    searchFeed this.props.state

handleSearchKeyPress this k =
    case keyEventToKeyCode k of
        Escape -> hideSearchInput this.props.state
        Enter  -> hideSearchInput this.props.state
        x      -> pure unit

listenSearchInputKeys state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"

    let keyCodesS = keyEventToKeyCode <$> bodyKeys

    (filterRx ((==) F3)     keyCodesS) ~> \_ -> showSearchInput state
    (filterRx ((==) Escape) keyCodesS) ~> \_ -> hideSearchInput state


searchInputComponent :: ComponentClass { state :: RefVal State } {}
searchInputComponent = createClass spec { displayName = "searchInputComponent"
                                        , render = renderFun }
    where
    renderFun this = do
      State { searchInput = (SearchInput { visible  = visible
                                         , value    = value } ) } <- readState this.props.state

      pure $
          D.div { className: "search-tweet"
                , onContextMenu: callEventHandler stopPropagation
                , onClick: callEventHandler stopPropagation
                , style: { display: if visible then "block" else "none"
                         , height: "155px"
                         , width: "100%"
                         , position: "fixed"
                         , top: "50%"
                         , "overflow": "hidden"
                         , "background-color": "rgba(0,0,0,0.95)"
                         , "box-shadow": "rgb(169, 169, 169) 0px 10px 50px"
                         , color: "white"
                         , transform: "translateY(-140px)"
                         , "-webkit-transform": "translateY(-140px)"
                         }} [
                    D.div {style: { "color": "white"
                                  , "width": "688px"
                                  , "padding": "20px"
                                  , "text-align": "left"
                                  , "margin": "auto" }} [D.rawText "Search"]

                  , D.input { type: "text"
                            , id: "search-input-id"
                            , value: value
                            , style: { width: "597px"
                                     , "font-size": "25px"
                                     , padding: "7px 11px 7px 11px" }
                            , onChange: eventHandler this handleChange
                            , onKeyUp: eventHandler this handleSearchKeyPress} []

                  , D.button { className: "writer-button nok"
                             , onClick: hideSearchInput this.props.state } [D.rawText "⨯"]

                  ]