module UI.UserInfo where

import Data.Maybe
import Control.Monad.Eff (Eff(..))
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


loadUserInfo state author@(Author {screen_name=sn}) = do
  let url = "/userinfo?sn=" ++ sn

  disableHistoryButton state
  clearUserInfo state
  showUserInfo state

  (rioGet url) ~> handler
  pure unit

  where
  handler resp = case (fromResponse resp) of
      ResponseError {errTitle = t, errMessage = m} -> do
          enableHistoryButton state
          setMessage state $ errorM m

      ResponseUserinfo {uiTitle = t, uiData = userinfo} -> do
          enableHistoryButton state
          s <- readState state
          writeState state (s # userInfoL .~ (UserInfo { visible: true
                                                       , userdata: Just userinfo }))

      -- TODO handle other cases

clearUserInfo :: forall eff. RefVal State
                          -> Eff (ref :: Ref | eff) Unit
clearUserInfo state = do
    s <- readState state
    writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoUserdataL .~ Nothing ) )

showUserInfo :: forall eff. RefVal State
                         -> Eff (ref :: Ref | eff) Unit
showUserInfo state = do
    s <- readState state
    writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoVisibleL .~ true ) )

hideUserInfo :: forall eff. RefVal State
                         -> Eff (ref :: Ref | eff) Unit
hideUserInfo state = do
    s <- readState state
    writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoVisibleL .~ false ) )

listenUserInfoKeys state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"

    let keyCodesS = keyEventToKeyCode <$> bodyKeys

    (filterRx ((==) Escape) keyCodesS) ~> \_ -> hideUserInfo state


instance asHtmlUser :: AsHtml User where
    asHtml s (User u) = D.ul { style: { display: "block"
                                       , width: "600px"
                                       , "text-align": "left"
                                       , margin: "auto"
                                       , "overflow": "auto"
                                       , padding: "20px"
                                       , height: "100%" } }
        [ D.li {style: { margin: "0px", padding: "5px" }} [D.span {style: {"font-size": "200%"}} [D.rawText u.userName]]
        , D.li {style: { margin: "0px", padding: "5px" }} [D.span {} [D.rawText $ "@" ++ u.userScreenName]]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userProfileImageURL of
                      Nothing -> D.rawText ""
                      Just url -> D.img {src: url} []]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userDescription of
                      Nothing -> D.rawText ""
                      Just desc -> D.rawText desc]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userURL of
                      Nothing -> D.rawText ""
                      Just url -> D.a {style: {color: "white"}, href: url, target: "_blank"} [D.rawText url]]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userLocation of
                      Nothing -> D.rawText ""
                      Just loc -> D.rawText loc]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userTimeZone of
                      Nothing -> D.rawText ""
                      Just tz -> D.rawText tz]
        , D.li {style: { margin: "0px", padding: "5px" }} [D.rawText $ "Registered on " ++ u.userCreatedAt]
        , D.li {style: { margin: "0px", padding: "5px" }} [D.rawText $ show u.userFollowersCount ++ " followers, " ++
                               show u.userFriendsCount ++ " friends, " ++
                               show u.userStatusesCount ++ " tweets" ]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userFollowing of
                      Nothing -> D.rawText ""
                      Just fol -> D.rawText $ if fol then "Already following"
                                                     else "Not following"]
        ]


userInfo :: ComponentClass { state :: RefVal State } {}
userInfo = createClass spec { displayName = "Messages", render = renderFun } where
  renderFun this = do
      State { userInfo = (UserInfo { visible  = visible
                                   , userdata = mbUser } ) } <- readState this.props.state

      pure $
          D.div { className: "user-info"
                , onContextMenu: callEventHandler stopPropagation
                , style: { display: if visible then "block" else "none"
                         , height: "440px"
                         , width: "100%"
                         , position: "fixed"
                         , top: "50%"
                         , "overflow": "hidden"
                         , "background-color": "rgba(0,0,0,0.95)"
                         , color: "white"
                         , transform: "translateY(-240px)"
                         } } [

                    case mbUser of
                        Nothing -> D.rawText "No user info available yet"
                        Just user -> asHtml this.props.state user

                  , D.button { className: "writer-button nok"
                              , style: { position: "absolute"
                                       , top: "31px"
                                       , transform: "translateX(240px)"
                                       }
                              , onClick: hideUserInfo this.props.state } [D.rawText "⨯"]
                  ]