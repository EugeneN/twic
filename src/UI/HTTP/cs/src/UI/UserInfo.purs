module UI.UserInfo where

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
                                                       , followRequestActive: false
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

getFollowUrl sn = "/follow?sn=" ++ sn
getUnfollowUrl sn = "/unfollow?sn=" ++ sn

handleFollow state sn ev = do
  stopPropagation ev
  handleFollowUnfollow state (getFollowUrl sn)
  pure unit

handleUnfollow state sn ev = do
  stopPropagation ev
  handleFollowUnfollow state (getUnfollowUrl sn)
  pure unit

handleFollowUnfollow state url = do
  disableHistoryButton state
  s <- readState state
  writeState state (s # userInfoL .~ (UserInfo { visible: true
                                               , followRequestActive: true
                                               , userdata: ((s ^. userInfoL) ^. userInfoUserdataL) }))
  (rioGet url) ~> handler
  pure unit

  where
  handler s = case (fromResponse s) of
      ResponseError {errTitle = t, errMessage = m} -> do
          enableHistoryButton state
          setMessage state $ errorM m

      ResponseUserinfo {uiTitle = t, uiData = userinfo} -> do
          enableHistoryButton state
          --setMessage state $ successM m

          s <- readState state
          writeState state (s # userInfoL .~ (UserInfo { visible: true
                                                       , followRequestActive: false
                                                       , userdata: Just userinfo }))

runEff action = runPure (unsafeInterleaveEff action)

followButton state user = do
  State {userInfo = (UserInfo {followRequestActive = fra})} <- readState state
  pure $ if not fra
    then D.button { style: {"margin-left": "8px"}
                           , onClick: callEventHandler $ handleFollow state user.userScreenName }
                  [D.rawText "Follow"]
    else D.img { src: "/snake-loader.gif"
               , style: {margin: "1px 10px 1px 10px"} } []

unfollowButton state user = do
  State {userInfo = (UserInfo {followRequestActive = fra})} <- readState state
  pure $ if not fra
    then D.button { style: {"margin-left": "8px"}
                           , onClick: callEventHandler $ handleUnfollow state user.userScreenName }
                  [D.rawText "Unfollow"]
    else D.img { src: "/snake-loader.gif"
               , style: {margin: "1px 10px 1px 10px"} } []

avatarUrl src = stringReplace src "_normal." "_400x400."

instance asHtmlUser :: AsHtml User where
    asHtml s (User u) = D.ul { style: { display: "block"
                                       , width: "600px"
                                       , "text-align": "left"
                                       , margin: "auto"
                                       , "overflow": "auto"
                                       , padding: "20px"
                                       --, color: "#" ++ u.userProfileTextColor
                                       , "background-color": "rgba(0,0,0,0.3)"
                                       , height: "100%" } }
        [ D.li {style: { margin: "0px", padding: "5px" }}
              [D.span {style: {"font-size": "200%"}}
                  [ D.rawText u.userName
                  , if u.userVerified then (D.span {style: {color: "blue"}} [D.rawText " •"])
                                      else (D.rawText "")
                  , if u.userProtected then (D.span {style: {color: "red"}} [D.rawText " •"])
                                       else (D.rawText "") ]]
        , D.li {style: { margin: "0px", padding: "5px" }}
              [D.a { href: "https://twitter.com/" ++ u.userScreenName
                   , style: {color: "lightgrey"}
                   , target: "_blank"} [D.rawText $ "@" ++ u.userScreenName]]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userProfileImageURL of
            Nothing -> D.rawText ""
            Just url -> D.img { src: avatarUrl url
                              , style: {width: "100px"}} []]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userDescription of
            Nothing -> D.rawText ""
            Just desc -> D.rawText desc]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userURL of
            Nothing -> D.rawText ""
            Just url -> D.a { style: { color: "white"
                                     , "text-decoration": "underline"}
                            , href: url
                            , target: "_blank"} [D.rawText url]]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userLocation of
            Nothing -> D.rawText ""
            Just loc -> D.rawText loc]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userTimeZone of
            Nothing -> D.rawText ""
            Just tz -> D.rawText $ tz ++ " timezone"]
        , D.li {style: { margin: "0px", padding: "5px" }} [D.rawText $ "Registered on " ++ u.userCreatedAt]
        , D.li {style: { margin: "0px", padding: "5px" }} [
            D.rawText $ show u.userFollowersCount ++ " followers, " ++
                        show u.userFriendsCount ++ " friends, " ++
                        show u.userStatusesCount ++ " tweets" ]
        , D.li {style: { margin: "0px", padding: "5px" }} [case u.userFollowing of
            Nothing -> D.rawText "Following status unknown"
            Just fol -> if fol
              then D.span {} [ D.rawText "Already following"
                             , (runEff $ unfollowButton s u) ]
              else D.span {} [ D.rawText "Not following"
                             , (runEff $ followButton s u) ]]
        ]

userInfo :: ComponentClass { state :: RefVal State } {}
userInfo = createClass spec { displayName = "Messages", render = renderFun } where
  renderFun this = do
      State { userInfo = (UserInfo { visible  = visible
                                   , userdata = mbUser } ) } <- readState this.props.state

      pure $
          D.div { className: "user-info"
                , onContextMenu: callEventHandler stopPropagation
                , onClick: callEventHandler stopPropagation
                , style: { display: if visible then "block" else "none"
                         , height: "440px"
                         , width: "100%"
                         , position: "fixed"
                         , top: "50%"
                         , "overflow": "hidden"
                         , "background-color": case mbUser of
                                                  Nothing -> "rgba(0,0,0,0.95)"
                                                  Just (User u) -> case u.userProfileBackgroundColor of
                                                    Nothing -> "rgba(0,0,0,0.95)"
                                                    Just c -> "#" ++ c
                          , "background-image": case mbUser of
                                                  Nothing -> "none"
                                                  Just (User u) -> case u.userProfileBannerURL of
                                                      Nothing -> "none"
                                                      Just src -> "url("++src++")"
                          , "background-size": case mbUser of
                                                  Nothing -> "auto"
                                                  Just (User u) -> case u.userProfileBannerURL of
                                                      Nothing -> "auto"
                                                      Just src -> "cover"
                         , color: "white"
                         , transform: "translateY(-240px)"
                         } } [

                    case mbUser of
                        Nothing -> D.img { src: "/snake-loader-darkbg.gif"
                                         , style: { position: "relative"
                                                  , top: "200px" } } []
                        Just user -> asHtml this.props.state user

                  --, D.button { className: "writer-button nok"
                  --            , style: { position: "absolute"
                  --                     , top: "31px"
                  --                     , transform: "translateX(240px)"
                  --                     }
                  --            , onClick: hideUserInfo this.props.state } [D.rawText "⨯"]
                  ]