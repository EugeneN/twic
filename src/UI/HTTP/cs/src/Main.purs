module Main where

import Debug.Trace
import Control.Monad.Eff.Ref
import Control.Monad.Eff (Eff(..))
import Config
import Types
import Utils
import Core
import UI.Feed (startWsClient, listenFeedKeys, showNewTweets, listenHistoryEvents)
import UI.LoaderIndicator (showLoader, hideLoader)
import UI.TweetWriter (listenWriteKeys)
import UI.RootLayout (renderRootLayout)




main = do
    trace "hello there"

    state <- newRef initialState
    rl <- renderRootLayout "root" state

    listenState state rl
    listenWriteKeys state
    listenFeedKeys state

    --listenHistoryEvents state

    startWsClient state

    pure unit

    where
    listenState state rl = stateObservable ~> (\_ -> do
        trace "Got state update"
        --renderRootLayout "root" state
        setProps rl state
        pure unit)


