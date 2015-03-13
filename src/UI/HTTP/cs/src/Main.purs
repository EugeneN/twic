module Main where

import Debug.Trace
import Control.Monad.Eff.Ref
import Control.Monad.Eff (Eff(..))
import Config
import Types
import Utils
import Core
import UI.Feed (startWsClient, listenFeedKeys, showNewTweets, listenHistoryEvents)
import UI.TweetWriter (listenWriteKeys)
import UI.RootLayout (renderRootLayout)
import UI.UserInfo (listenUserInfoKeys)




main = do
    trace "hello there"

    state <- newRef initialState
    rl <- renderRootLayout "root" state

    listenState state rl
    listenWriteKeys state
    listenUserInfoKeys state
    listenFeedKeys state

    --listenHistoryEvents state

    startWsClient state

    pure unit

    where
    listenState state rl = stateObservable ~> (\_ -> do
        trace $ "Got state update" ++ toString state
        --renderRootLayout "root" state
        setProps rl state
        pure unit)


