module UI.MyInfo where

import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec)
import React.Types (Component(), ComponentClass(),  React())
import Types
import Core (readState, writeState)
import Data.Array
import Control.Monad.Eff.Ref
import Utils
import Optic.Core ( (.~), (^.))
import Core
import UI.Types
import qualified Control.Monad.JQuery as J
import qualified Rx.Observable as Rx
import Rx.JQuery


--loadMyInfo state = do
--  let url = "/myinfo?sn=" ++ sn
----
--  disableHistoryButton state
--  showMyInfo state
----
--  (rioGet url) ~> handler
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

showMyInfo :: forall eff. RefVal State -> Eff (ref :: Ref | eff) Unit
showMyInfo state = do
    s <- readState state
    writeState state (s # myInfoL .~ ( (s ^. myInfoL) # myInfoVisibleL .~ true ) )

hideMyInfo :: forall eff. RefVal State -> Eff (ref :: Ref | eff) Unit
hideMyInfo state = do
    s <- readState state
    writeState state (s # myInfoL .~ ( (s ^. myInfoL) # myInfoVisibleL .~ false ) )

listenMyInfoKeys state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"
    let keyCodesS = keyEventToKeyCode <$> bodyKeys
    (filterRx ((==) Insert) keyCodesS) ~> \_ -> showMyInfo state
    (filterRx ((==) Escape) keyCodesS) ~> \_ -> hideMyInfo state


myInfo :: ComponentClass { state :: RefVal State } {}
myInfo = createClass spec { displayName = "Messages", render = renderFun } where
  renderFun this = do
      State { myInfo = (MyInfo { visible  = visible } ) } <- readState this.props.state

      pure $
          D.div { className: "user-info"
                , onContextMenu: callEventHandler stopPropagation
                , onClick: callEventHandler stopPropagation
                , style: { display: if visible then "block" else "none"
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
                         } } [

                    D.ul {} [ D.li {} [D.rawText "Me"]
                            , D.li {} [D.rawText "Friends"]
                            , D.li {} [D.rawText "Favorites"]
                            ]
                  ]