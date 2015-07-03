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
import Data.String (indexOf, toLower)

showSearchInput :: forall eff. Ref State
                            -> Eff (ref :: REF, dom :: DOM, react :: React, trace :: Trace | eff) Unit
showSearchInput state = do
    s <- readState state
    writeState state (s # searchInputL .~ SearchInput { visible: true -- resetting all fields
                                                      , value: "" })
    forkPostpone (setFocus "search-input-id") 50

hideSearchInput :: forall eff. Ref State
                            -> Eff (ref :: REF, dom :: DOM, react :: React, trace :: Trace | eff) Unit
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
tweetElementMatch st (AtUsername s) = strMatch st (toLower s)
tweetElementMatch st (Link s)       = strMatch st (toLower s)
tweetElementMatch st (PlainText s)  = strMatch st (toLower s)
tweetElementMatch st (Hashtag s)    = strMatch st (toLower s)
tweetElementMatch st (Retweet s)    = false
tweetElementMatch st (Spaces s)     = false
tweetElementMatch st (Unparsable s) = strMatch st (toLower s)

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

    filterFeed :: String -> Array Tweet -> Array Tweet
    filterFeed st fd = filter (searchWholeTweet st) fd

    searchWholeTweet :: String -> Tweet -> Boolean
    searchWholeTweet searchTerm (Tweet {text=body}) =
        strMatch (toLower searchTerm) (intercalate "" (toLower <<< asText <$> body))

    searchTweet :: String -> Tweet -> Boolean
    searchTweet st (Tweet {text=body}) =
        any ((==) true) $ (tweetElementMatch (toLower st)) <$> body

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

    (filterRx ((==) F3)     keyCodesS) `Rx.subscribe` \_ -> showSearchInput state
    (filterRx ((==) Escape) keyCodesS) `Rx.subscribe` \_ -> hideSearchInput state


searchInputComponent :: ComponentClass { state :: REFVal State } {}
searchInputComponent = createClass spec { displayName = "searchInputComponent"
                                        , render = renderFun }
    where
    renderFun this = do
      State { searchInput = (SearchInput { visible  = visible
                                         , value    = value } ) } <- readState this.props.state

      pure $
          D.div [ P.className "search-tweet"
                , P.onDoubleClick $ callEventHandler stopPropagation
                , P.onClick $ callEventHandler stopPropagation
                , P.style { display: if visible then "block" else "none"
                          , height: "205px"
                          , width: "100%"
                          , position: "fixed"
                          , top: "50%"
                          , "overflow": "hidden"
                          , "background-color": "rgba(0,0,0,0.95)"
                          , "box-shadow": "rgb(169, 169, 169) 0px 10px 50px"
                          , color: "white"
                          , transform: "translateY(-140px)"
                          , "-webkit-transform": "translateY(-140px)"
                          }] [
                    D.div [ P.className "popup-panel-label"
                          , P.style { "color": "white"
                                    , "width": "688px"
                                    , "padding": "20px"
                                    , "text-align": "left"
                                    , "margin": "auto" }] [D.text "Search"]

                  , D.input [ P.type "text"
                            , P.id "search-input-id"
                            , P.value value
                            , P.style { width: "597px"
                                      , "font-size": "25px"
                                      , padding: "7px 11px 7px 11px" }
                            , P.onChange $ eventHandler this handleChange
                            , P.onKeyUp $ eventHandler this handleSearchKeyPress ] []

                  , D.button [ P.className "writer-button nok"
                             , P.onClick $ hideSearchInput this.props.state ] [D.text "⨯"]

                  ]
