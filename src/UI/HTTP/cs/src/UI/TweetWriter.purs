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
import Data.Foldable (foldl, intercalate)


getAuthorsNames (Tweet { user = Author {name = n1}, retweet = rt}) =
  let names = case rt of
                  Nothing -> [n1]
                  Just (Tweet {user =  Author {name = n2}}) -> [n1, n2]
  in intercalate  ", " names

getReplyToAuthors (Tweet {user = Author {screen_name = sn1}, retweet = rt}) =
  let names = case rt of
                  Nothing -> [sn1]
                  Just (Tweet {user =  Author {screen_name = sn2}}) -> [sn1, sn2]
  in foldl (++) "" ((\n -> "@" ++ n ++ " ") <$> names)

showReplyInput :: forall eff. Ref State
                           -> Maybe Tweet
                           -> Eff (ref :: REF, dom :: DOM, react :: React, trace :: Trace | eff) Unit
showReplyInput state mbTweet = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: true
                                                    , disabled: false
                                                    , value: case mbTweet of
                                                        Nothing -> ""
                                                        Just tweet -> getReplyToAuthors tweet
                                                    , replyTo: mbTweet })

    forkPostpone (setFocus "write-input-id") 50


showWriteInput :: forall eff. Ref State
                           -> Eff (ref :: REF, dom :: DOM, react :: React, trace :: Trace | eff) Unit
showWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ WriteInput { visible: true -- resetting all fields
                                                    , disabled: false
                                                    , value: ""
                                                    , replyTo: Nothing })

    forkPostpone (setFocus "write-input-id") 50

hideWriteInput :: forall eff. Ref State
                           -> Eff (ref :: REF, dom :: DOM, react :: React, trace :: Trace | eff) Unit
hideWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ ( (s ^. writeInputL) # writeInputVisibleL .~ false ) )

disableWriteInput state = do
    s <- readState state
    writeState state (s # writeInputL .~ ( (s ^. writeInputL) # writeInputDisabledL .~ true ) )

handleSubmitTweet :: forall eff. Ref State
                              -> Maybe Tweet
                              -> Eff (ref :: REF, react :: React, dom :: DOM, trace :: Trace | eff) Unit
handleSubmitTweet state reply = do
    s <- readState state
    case (s ^. writeInputL) ^. writeInputValueL of
        "" -> do
            trace $ "won't submit an empty tweet"
            setMessage state (errorM "Won't submit an empty tweet")
            pure unit

        text -> do
            disableWriteInput state
            disableHistoryButton state
            trace $ "gonna tweet this: " ++ text

            let url = case reply of
                        Nothing -> "/tweet?status=" ++ text
                        Just (Tweet {id_str = reply_id}) -> "/reply?status=" ++ text
                                                            ++ "&reply_to=" ++ reply_id

            (rioPost url Nothing) `Rx.subscribe` tweetResultHandler state
            pure unit

            where
            tweetResultHandler state resp = do
                hideWriteInput state
                enableHistoryButton state
                trace $ "tweeted " ++ show (resp :: AjaxResult)

                case (fromResponse resp) of
                    ResponseError {errTitle = t, errMessage = m} -> do
                        setMessage state $ errorM m

                    ResponseSuccess {okTitle = _, okFeedMessages = _} -> do
                        case reply of
                            Nothing -> setMessage state (successM "Tweeted :-)")
                            Just _  -> setMessage state (successM "Replied :-)")

                pure unit

handleChange this k = do
    s <- readState this.props.state
    writeState this.props.state (s # writeInputL .~ ( (s ^. writeInputL) # writeInputValueL .~ (value k.target) ) )

handleWriteKeyPress reply this k =
    case keyEventToKeyCode k of
        Escape -> hideWriteInput this.props.state
        Enter  -> handleSubmitTweet this.props.state reply
        x      -> pure unit

listenWriteKeys state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"

    let keyCodesS = keyEventToKeyCode <$> bodyKeys

    (filterRx ((==) F2)     keyCodesS) `Rx.subscribe` \_ -> showWriteInput state
    (filterRx ((==) Escape) keyCodesS) `Rx.subscribe` \_ -> hideWriteInput state


writeInputComponent :: ComponentClass { state :: REFVal State } {}
writeInputComponent = mkUI $ spec {} 	his -> do
    where
    renderFun this = do
      State { writeInput = (WriteInput { visible  = visible
                                       , disabled = disabled
                                       , value    = value
                                       , replyTo  = reply } ) } <- readState this.props.state

      pure $
          D.div [ P.className "write-tweet"
                , P.disabled disabled
                , P.onDoubleClick $ callEventHandler stopPropagation
                , P.onClick # callEventHandler stopPropagation
                , P.style { display: if visible then "block" else "none"
                          , "box-shadow": "rgb(169, 169, 169) 0px 10px 50px"
                          , height: case reply of
                                       Nothing -> "150px"
                                       Just _  -> "150px"
                          , transform: case reply of
                                           Nothing -> "translateY(-150px)"
                                           Just _  -> "translateY(-150px)"
                          , "-webkit-transform": case reply of
                                           Nothing -> "translateY(-150px)"
                                           Just _  -> "translateY(-150px)"
                         }] [
                    case reply of
                      Nothing    -> D.div [ P.className "popup-panel-label"
                                          , P.style { "color": "white"
                                                   , "width": "710px"
                                                   , "padding": "20px"
                                                   , "padding-top": "0px"
                                                   , "text-align": "left"
                                                   , "margin": "auto"}] [D.text "What's up?"]
                      Just tweet -> D.div [ P.className "popup-panel-label"
                                          , P.style { "color": "white"
                                                   , "width": "710px"
                                                   , "padding": "20px"
                                                   , "padding-top": "0px"
                                                   , "text-align": "left"
                                                   , "margin": "auto" }] [ D.text $ "Reply to " ++ (getAuthorsNames tweet)]

                  , D.input [ P.type "text"
                            , P.id "write-input-id"
                            , P.value value
                            , P.onChange $ eventHandler this handleChange
                            , P.onKeyUp $ eventHandler this (handleWriteKeyPress reply) ] []

                  , D.button [ P.className "writer-button ok"
                             , P.onClick (handleSubmitTweet this.props.state reply) ] [D.text "✓"]

                  , D.button [ P.className "writer-button nok"
                             , P.onClick $ hideWriteInput this.props.state ] [D.text "⨯"]]
