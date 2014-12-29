module Main where

import Debug.Trace
import Control.Monad.Eff.Ref
import Control.Monad.Eff (Eff(..))
import Config
import Types
import Utils
import Core
import UI.Feed (loadFeed, startWsClient)
import UI.LoaderIndicator (showLoader, hideLoader)
import UI.Messages (renderMessage)
import UI.TweetWriter (listenKeys)




main = do
    trace "hello there"
    state <- newRef initialState

    listenKeys

    loadFeed state
    startWsClient state

    pure unit


