module UI.TweetWriter where

import Debug.Trace
import Data.Maybe
import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec, eventHandler)
import React.Types (Component(), ComponentClass(),  React())

import qualified Rx.Observable as Rx
import Rx.JQuery
import qualified Control.Monad.JQuery as J

import Config (writeTweetContainerId, messagesId)
import Utils
import Types (AjaxResult())
import UI.Messages (renderMessage)


foreign import setFocus "function setFocus(id) { return function() { jQuery('#' + id).focus(); }}" :: forall eff. String -> Eff eff Unit
foreign import which "function which(ev) { return ev.which; }" :: J.JQueryEvent -> Number

handleShowWriteInput :: forall eff. Eff (dom :: DOM, react :: React, trace :: Trace | eff) Unit
handleShowWriteInput = do
    trace "handleShowWriteInput here"
    showWriteInput writeTweetContainerId
    setFocus "write-input-id"
    pure unit

handleShowWriteButton :: forall eff. Eff (dom :: DOM, react :: React, trace :: Trace | eff) Unit
handleShowWriteButton = do
    trace "handleShowWriteButton here"
    showWriteButton writeTweetContainerId
    pure unit

handleSubmitTweet :: forall eff. String -> Eff (react :: React, dom :: DOM, trace :: Trace | eff) Unit
handleSubmitTweet "" = do
    trace $ "won't submit an empty tweet"
    pure unit

handleSubmitTweet text = do
    trace $ "gonna tweet this: " ++ text
    let url = "/tweet?status=" ++ text
    (rioPost url Nothing) ~> tweetResultHandler
    pure unit

    where
    tweetResultHandler resp = do
        trace $ "tweeted " ++ show (resp :: AjaxResult)
        renderMessage messagesId "Tweeted :-)"
        showWriteButton writeTweetContainerId
        pure unit


handleWriteKeyPress this k = do
    case k.keyCode of
        27 -> handleShowWriteButton
        13 -> handleSubmitTweet $ value k.target
        _  -> pure unit

    pure unit

listenKeys = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"

    (filterRx (isKey 13) bodyKeys) ~> \_ -> handleShowWriteInput
    (filterRx (isKey 27) bodyKeys) ~> \_ -> handleShowWriteButton

    where isKey a x = which x == a


writeInputComponent :: ComponentClass {} {}
writeInputComponent = createClass spec { displayName = "writeInputComponent", render = renderFun }
    where
        renderFun this = pure $
            D.div {className: "write-tweet"} [
                    D.input { "type": "text"
                            , "id": "write-input-id"
                            , onKeyUp: eventHandler this handleWriteKeyPress } [] ]

writeButtonComponent :: ComponentClass {} {}
writeButtonComponent = createClass spec { displayName = "writeButtonComponent", render = renderFun }
    where
        renderFun this = pure $
            D.button { className: "show-write-tweet"
                     , "id": "write-tweet-id"
                     , onClick: handleShowWriteInput
                     , dangerouslySetInnerHTML: {__html: "&middot;&middot;&middot;"}} []

showWriteInput :: forall eff. String -> Eff (dom :: DOM, react :: React | eff) Component
showWriteInput targetId = renderComponentById (writeInputComponent {} []) targetId

showWriteButton :: forall eff. String -> Eff (dom :: DOM, react :: React | eff) Component
showWriteButton targetId = renderComponentById (writeButtonComponent {} []) targetId
