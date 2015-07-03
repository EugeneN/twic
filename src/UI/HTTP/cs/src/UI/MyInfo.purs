module UI.MyInfo where

import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Data.Array
import Control.Monad.Eff.Ref
import DOM (DOM(..))

import React hiding (readState, writeState)
import qualified React.DOM as D
import qualified React.DOM.Props as P

import Optic.Setter ((.~))
import Optic.Getter ((^.))
import Prelude

import Types
import Utils
import Core
import UI.Types
import qualified Rx.Observable as Rx
import qualified Data.Set as DS
import UI.UserInfo (loadUserInfo')

--loadMyInfo state = do
--  let url = "/myinfo?sn=" ++ sn
----
--  disableHistoryButton state
--  showMyInfo state
----
--  (rioGet url) `Rx.subscribe` handler
--  pure unit
----
--  where
--  handler resp = case (fromResponse resp) of
--      ResponseError {errTitle = t, errMessage = m} -> do
--          enableHistoryButton state
--          setMessage state $ errorM m
----
--      ResponseUserinfo {myTitle = t, myData = myinfo} -> do
--          enableHistoryButton state
--          s <- readState state
--          writeState state (s # myInfoL .~ (MyInfo { visible: true
--                                                   , friends: ?
--                                                   , userInfo: ? }))

      -- TODO handle other cases

showMyInfo :: forall eff. Ref State -> Eff (ref :: REF | eff) Unit
showMyInfo state = do
    s <- readState state
    writeState state (s # myInfoL .~ ( (s ^. myInfoL) # myInfoVisibleL .~ true ) )

hideMyInfo :: forall eff. Ref State -> Eff (ref :: REF | eff) Unit
hideMyInfo state = do
    s <- readState state
    writeState state (s # myInfoL .~ ( (s ^. myInfoL) # myInfoVisibleL .~ false ) )

listenMyInfoKeys state = do
    bodyKeys <- fromEvent "keyup"
    let keyCodesS = keyEventToKeyCode <$> bodyKeys
    (Rx.filter ((==) Insert) keyCodesS) `Rx.subscribe` \_ -> showMyInfo state
    (Rx.filter ((==) Escape) keyCodesS) `Rx.subscribe` \_ -> hideMyInfo state

handleMyInfoContextMenu state ev = do
  stopPropagation ev
  resetContextMenu state

  hideMyInfo state

  State {account = (Account { settings = mbsettings }) } <- readState state

  case mbsettings of
    Nothing -> setMessage state (errorM "Account settings not available")
    Just (TASettings { accScreenName = sn }) -> loadUserInfo' state sn

  pure unit

myInfo = mkUI $ spec {} \this -> do
      State { myInfo  = (MyInfo { visible  = visible })
            , account = (Account { friends = friends
                                 , settings: settings }) } <- getProps this

      let username = case settings of
                         Nothing -> "Username unknown"
                         Just (TASettings {accScreenName = asn}) -> asn

          friendsStr = case friends of
                         Nothing -> "Number of friends not known"
                         Just x  -> (show $ length $ DS.toList x) ++ " friends"

      pure $
          D.div [ P.className "user-info"
                , P.onDoubleClick: callEventHandler stopPropagation
                , P.onClick callEventHandler stopPropagation
                , P.style { display: if visible then "block" else "none"
                          , height: "460px"
                          , width: "100%"
                          , position: "fixed"
                          , top: "50%"
                          , "overflow": "hidden"
                          , "background-color": "rgba(0,0,0,0.95)"
                          , "box-shadow": "rgb(169, 169, 169) 0px 10px 50px"
                          , color: "white"
                          , transform: "translateY(-240px)"
                          , "-webkit-transform": "translateY(-240px)"
                          }
                ] [ D.ul [] [ D.li [ P.className "popup-panel-label"
                                   , P.style { cursor: "pointer" }
                                   , P.onClick (callEventHandler $ handleMyInfoContextMenu this.props.state)
                                   ] [ D.text username ]

                            , D.li [P.className "popup-panel-label"] [ D.text "Friends: "
                                                                     , D.text friendsStr ]
                            , D.li [ P.className "popup-panel-label"] [ D.text "Favorites" ]
                            ]
                  ]
