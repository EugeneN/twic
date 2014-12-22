module Main where

import Debug.Trace
import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))

import Data.DOM.Simple.Types (HTMLElement(..))
import Data.DOM.Simple.Element (getAttribute)

import Rx.JQuery
import qualified Rx.Observable as Rx

import qualified Control.Monad.JQuery as J
import Data.Either
import Data.Monoid

import qualified Network.XHR as X
import qualified Network.XHR.Internal as XI
import qualified Network.XHR.Types as XT

import Types
import Utils
import Core
import UI

import qualified Lib.WebSocket as WS

checkButtonContainer = "refresh"
checkButton = "button"

rxTest :: forall e. Eff (trace :: Trace, dom :: DOM | e) Unit
rxTest = do
    spansStream <- J.select "span" >>= onAsObservable "mouseover"
    imgsStream <- J.select "img" >>= onAsObservable "mouseover"

    let s = spansStream <> imgsStream
--    s ~> (toString >>> trace)

    let s2 = extractCoords <$> s
    s2 ~> (toString >>> trace)

foreign import filterRx
    """
    function filterRx(f){
        return function(observable) {
            return observable.filter(f)
        }
    }
    """ :: forall a. (a -> Boolean) -> Rx.Observable a -> Rx.Observable a

foreign import byId
    """
    function byId(idstr){
        return function(event) {
            return event.target.id === idstr;
        }
    }
    """ :: forall a. String -> a -> Boolean

--startAppBus :: forall e. Eff (trace :: Trace, dom :: DOM | e) Unit
startAppBus = do
    bodyClicks <- J.select "body" >>= onAsObservable "click"
    (filterRx (byId "load-new-tweets-id") bodyClicks) ~> \_ -> loadTweets

    pure unit

--checkNewTweets :: forall e. Eff (trace :: Trace, dom :: DOM | e) Unit
checkNewTweets = do
    (getIntervalStream oneMinute) ~> checkTweets
    pure unit

    where
        checkUrl = "/check"
        checkTweets _ =  do
            (rioGet checkUrl) ~> handleCheck
            pure unit

        handleCheck res = do
            trace $ "got update " ++ toString res

            case (fromCheckResponse res) of
                CheckResponse { unreadTitle = _, unreadCount = -1 } -> do
                    trace "*** check failed"
                    pure unit

                CheckResponse { unreadTitle = t, unreadCount = c } -> do
                    setTitle t
                    renderCheckButton checkButtonContainer c
                    pure unit

            pure unit

--loadTweets :: forall e. Eff (trace :: Trace, dom :: DOM | e) Unit
loadTweets = do
    setTitle "Loading..."
    showLoader containerId
    (rioGet url) ~> handleUpdate

    pure unit

    where
        url = "/update"
        containerId = "container"
        messagesId = "messages"

        handleUpdate s = case (fromResponse s) of
            ResponseError {errTitle = t, errMessage = m} -> do
                setTitle t
                renderMessage messagesId m
                hideLoader containerId
                pure unit

            ResponseSuccess {okTitle = t', okTweets = ts} -> do
                setTitle "0 new tweets"
                renderMessage messagesId ""
                renderTweets containerId ts
                renderCheckButton checkButtonContainer 0
                scrollToTop
                pure unit

socketUrl = "ws://localhost:3000"

eitherDecode msg = case msg of
    "ping"            -> Right Heartbeat
    _ | isNumeric msg -> Right $ UnreadCount $ readInt msg
    _                 -> Left $ "Unexpected message: " ++ toString msg

data Message = Heartbeat | UnreadCount Number


startWsClient = do
    socket <- WS.mkWebSocket socketUrl

    trace "ws connected"

    WS.onMessage socket onMessage
    WS.onError   socket onError
    WS.onClose   socket onClose

    pure unit

    where
        onMessage msg = case eitherDecode msg of
            Right (UnreadCount n) -> do
                setTitle $ show n ++ " new tweets"
                renderCheckButton checkButtonContainer n
                pure unit

            Right Heartbeat -> do
                trace "got ping"
                pure unit

            Left x -> do
                trace $ "can't decode ws message: " ++ x
                pure unit

        onError = do
            trace $ "ws error"
            pure unit

        onClose = do
            trace "ws closed"
            startWsClient
            pure unit

main = do
    trace "hello there"

    startAppBus

    loadTweets

    -- checkNewTweets

    startWsClient

    rxTest

    return unit


