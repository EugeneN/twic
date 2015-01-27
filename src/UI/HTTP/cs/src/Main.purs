module Main where

import Debug.Trace
import Control.Monad.Eff.Ref
import Control.Monad.Eff (Eff(..))
import Config
import Types
import Utils
import Core
import UI.Feed (loadFeed, startWsClient, listenFeedKeys, loadTweetsFromState)
import UI.LoaderIndicator (showLoader, hideLoader)
import UI.Messages (renderMessage)
import UI.TweetWriter (listenWriteKeys)




main = do
    trace "hello there"
    state <- newRef initialState

    listenWriteKeys
    listenFeedKeys state

    loadTweetsFromState state
    startWsClient state

    pure unit


