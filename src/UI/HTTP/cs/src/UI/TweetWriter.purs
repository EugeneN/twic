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


showReplyInput :: forall eff. RefVal State
                           -> Maybe Tweet
                           -> Eff (ref :: Ref, dom :: DOM, react :: React, trace :: Trace | eff) Unit
showReplyInput state tweet = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: true, disabled: false, replyTo: tweet })

    forkPostpone (setFocus "write-input-id") 50


showWriteInput :: forall eff. RefVal State
                           -> Eff (ref :: Ref, dom :: DOM, react :: React, trace :: Trace | eff) Unit
showWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: true, disabled: false, replyTo: Nothing })

    forkPostpone (setFocus "write-input-id") 50

hideWriteInput :: forall eff. RefVal State
                           -> Eff (ref :: Ref, dom :: DOM, react :: React, trace :: Trace | eff) Unit
hideWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: false, disabled: false, replyTo: Nothing })

disableWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: true -- TODO use prev value
                                                    , disabled: true
                                                    , replyTo: Nothing }) -- TODO use prev value

handleSubmitTweet :: forall eff. RefVal State
                              -> Maybe Tweet
                              -> String
                              -> Eff (ref :: Ref, react :: React, dom :: DOM, trace :: Trace | eff) Unit
handleSubmitTweet state _ "" = do
    trace $ "won't submit an empty tweet"
    setMessage state (errorM "Won't submit an empty tweet")
    pure unit

handleSubmitTweet state reply text = do
    disableWriteInput state
    disableHistoryButton state
    trace $ "gonna tweet this: " ++ text

    let url = case reply of
                Nothing -> "/tweet?status=" ++ text
                Just (Tweet {id_str = reply_id}) -> "/reply?status=" ++ text ++ "&reply_to=" ++ reply_id

    (rioPost url Nothing) ~> tweetResultHandler state
    pure unit

    where
    tweetResultHandler state resp = do
        hideWriteInput state
        enableHistoryButton state
        trace $ "tweeted " ++ show (resp :: AjaxResult)
        -- TODO check error response

        case reply of
            Nothing -> setMessage state (successM "Tweeted :-)")
            Just _  -> setMessage state (successM "Replied :-)")

        pure unit


handleWriteKeyPress reply this k =
    case keyEventToKeyCode k of
        Escape -> hideWriteInput this.props.state
        Enter  -> handleSubmitTweet this.props.state reply (value k.target)
        _      -> pure unit

listenWriteKeys state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"

    let keyCodesS = keyEventToKeyCode <$> bodyKeys

    (filterRx ((==) F2) keyCodesS)     ~> \_ -> showWriteInput state
    (filterRx ((==) Escape) keyCodesS) ~> \_ -> hideWriteInput state


writeInputComponent :: ComponentClass { state :: RefVal State } {}
writeInputComponent = createClass spec { displayName = "writeInputComponent", render = renderFun }
    where
      getAuthor (Tweet { user = Author {name = username} }) = username
      getAuthorSN (Tweet { user = Author {screen_name = sn} }) = sn
      renderFun this = do
        State { writeInput = (WriteInput { visible = visible
                                         , disabled = disabled
                                         , replyTo: reply } ) } <- readState this.props.state

        pure $
            D.div { className: "write-tweet"
                  , disabled: disabled
                  , onContextMenu: stopPropagation
                  , style: { display: if visible then "block" else "none"
                           , height: case reply of
                                        Nothing -> "50px"
                                        Just _  -> "100px"
                           , transform: case reply of
                                            Nothing -> "translateY(-70px)"
                                            Just _  -> "translateY(-140px)"
                           }} [
                      case reply of
                        Nothing    -> D.div {} []
                        Just tweet -> D.div {style: { "color": "white"
                                                    , "width": "710px"
                                                    , "padding": "20px"
                                                    , "padding-top": "0px"
                                                    , "text-align": "left"
                                                    , "margin": "auto" }} [D.rawText $ "Reply to " ++ (getAuthor tweet)]

                    , D.input { "type": "text"
                              , "id": "write-input-id"
                              , "value": case reply of
                                            Nothing -> ""
                                            Just tweet -> "@" ++ getAuthorSN tweet ++ " "
                              , onKeyUp: eventHandler this (handleWriteKeyPress reply)  } []

                    , D.button { className: "writer-button ok"
                               , onClick: (handleSubmitTweet this.props.state reply "") } [D.rawText "✓"]

                    , D.button { className: "writer-button nok"
                               , onClick: hideWriteInput this.props.state } [D.rawText "⨯"]]
