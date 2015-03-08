module UI.TweetWriter where

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


showWriteInput :: forall eff. RefVal State
                           -> Eff (ref :: Ref, dom :: DOM, react :: React, trace :: Trace | eff) Unit
showWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: true, disabled: false })

    forkPostpone (setFocus "write-input-id") 50

hideWriteInput :: forall eff. RefVal State
                           -> Eff (ref :: Ref, dom :: DOM, react :: React, trace :: Trace | eff) Unit
hideWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: false, disabled: false })

disableWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: true -- TODO use prev value
                                                    , disabled: true })
        
handleSubmitTweet :: forall eff. RefVal State
                              -> String
                              -> Eff (ref :: Ref, react :: React, dom :: DOM, trace :: Trace | eff) Unit
handleSubmitTweet state "" = do
    trace $ "won't submit an empty tweet"
    setMessage state (errorM "Won't submit an empty tweet")
    pure unit

handleSubmitTweet state text = do
    disableWriteInput state -- TODO just diable here, hide in response handler
    trace $ "gonna tweet this: " ++ text
    let url = "/tweet?status=" ++ text

    (rioPost url Nothing) ~> tweetResultHandler state
    pure unit

    where
    tweetResultHandler state resp = do
        hideWriteInput state
        trace $ "tweeted " ++ show (resp :: AjaxResult)
        -- TODO check error response
        setMessage state (successM "Tweeted :-)")
        pure unit


handleWriteKeyPress this k = do
    case keyEventToKeyCode k of
        Escape -> hideWriteInput this.props.state
        Enter  -> handleSubmitTweet this.props.state $ value k.target
        _      -> pure unit

    pure unit

listenWriteKeys state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"

    let keyCodesS = keyEventToKeyCode <$> bodyKeys

    (filterRx ((==) F2) keyCodesS)     ~> \_ -> showWriteInput state
    (filterRx ((==) Escape) keyCodesS) ~> \_ -> hideWriteInput state


writeInputComponent :: ComponentClass { state :: RefVal State } {}
writeInputComponent = createClass spec { displayName = "writeInputComponent", render = renderFun }
    where
      renderFun this = do
        State { writeInput = (WriteInput { visible = visible
                                         , disabled = disabled }) } <- readState this.props.state

        pure $
            D.div { className: "write-tweet"
                  , disabled: disabled
                  , style: { display: if visible then "block" else "none" }} [
                      D.input { "type": "text"
                              , "id": "write-input-id"
                              , onKeyUp: eventHandler this handleWriteKeyPress  } []

                    , D.button { className: "writer-button ok"
                               , onClick: (handleSubmitTweet this.props.state "") } [D.rawText "✓"]

                    , D.button { className: "writer-button nok"
                               , onClick: hideWriteInput this.props.state } [D.rawText "⨯"]]
