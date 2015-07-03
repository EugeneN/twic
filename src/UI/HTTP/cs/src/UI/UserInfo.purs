module UI.UserInfo where

import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import DOM (DOM(..))

import React hiding (readState, writeState)
import qualified React.DOM as D
import qualified React.DOM.Props as P

import Types
import Core (readState, writeState)
import Data.Array
import Control.Monad.Eff.Ref
import Utils

import Optic.Setter ((.~))
import Optic.Getter ((^.))
import Prelude

import Core
import UI.Types
import qualified Rx.Observable as Rx

loadUserInfo state author@(Author {screen_name=sn}) = loadUserInfo' state sn

loadUserInfo' state sn = do
  let url = "/userinfo?sn=" ++ sn

  disableHistoryButton state
  clearUserInfo state
  showUserInfo state

  (rioGet url) `Rx.subscribe` handler
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

clearUserInfo :: forall eff. Ref State
                          -> Eff (ref :: REF | eff) Unit
clearUserInfo state = do
    s <- readState state
    writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoUserdataL .~ Nothing ) )

showUserInfo :: forall eff. Ref State
                         -> Eff (ref :: REF | eff) Unit
showUserInfo state = do
    s <- readState state
    writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoVisibleL .~ true ) )

hideUserInfo :: forall eff. Ref State
                         -> Eff (ref :: REF | eff) Unit
hideUserInfo state = do
    s <- readState state
    writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoVisibleL .~ false ) )

listenUserInfoKeys state = do
    bodyKeys <- fromEvent "keyup"

    let keyCodesS = keyEventToKeyCode <$> bodyKeys

    (Rx.filter ((==) Escape) keyCodesS) `Rx.subscribe` \_ -> hideUserInfo state

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
  (rioGet url) `Rx.subscribe` handler
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

followButtonLoaderStyle = { margin: "0px 10px 0px 10px"
                          , position: "relative"
                          , top: "4px" }

followButton state user = do
  State {userInfo = (UserInfo {followRequestActive = fra})} <- readState state
  pure $ if not fra
    then D.button [ P.style { "margin-left": "8px"
                            , "background-color": "#B0E57C"
                            , "cursor": "pointer"
                            , "border-radius": "10px"
                            , "border": "0px solid green" }
                  , P.onClick (callEventHandler $ handleFollow state user.userScreenName)
                  ] [D.text "Follow"]
    else D.img [ P.src: "/snake-loader.gif"
               , P.style followButtonLoaderStyle ] []

unfollowButton state user = do
  State {userInfo = (UserInfo {followRequestActive = fra})} <- readState state
  pure $ if not fra
    then D.button [ P.style { "margin-left": "8px"
                           , "background-color": "#FFAEAE"
                           , "cursor": "pointer"
                           , "border-radius": "10px"
                           , "border": "0px solid red" }
                           , P.onClick callEventHandler $ handleUnfollow state user.userScreenName
                  ] [D.text "Unfollow"]
    else D.img [ P.src: "/snake-loader.gif"
               , P.style followButtonLoaderStyle ] []

avatarUrl src = stringReplace src "_normal." "_400x400."

instance asHtmlUser :: AsHtml User where
    asHtml s (User u) = D.ul [ P.style { "display": "block"
                                       , "width": "600px"
                                       , "text-align": "left"
                                       , "margin": "auto"
                                       , "overflow": "hidden"
                                       , "padding": "20px"
                                       --, "color": "#" ++ u.userProfileTextColor
                                       , "background-color": "rgba(0,0,0,0.3)"
                                       , "height": "420px" } ]
        [ D.li [ P.style { "margin": "0px"
                          , "padding": "5px"
                          , "padding-top": "0px"
                          , "margin-top": "-5px" } ]
              [D.span [ P.style {"font-size": "200%"} ]
                  [ D.text u.userName
                  , if u.userVerified then (D.span [ P.style {color: "blue"}] [D.text " •"])
                                      else (D.text "")
                  , if u.userProtected then (D.span [ P.style {color: "red"}] [D.text " •"])
                                       else (D.text "") ]]
        , D.li [ P.style { margin: "0px", padding: "5px" }]
              [D.a { href: "https://twitter.com/" ++ u.userScreenName
                   , P.style {color: "lightgrey"}
                   , target: "_blank"} [D.text $ "@" ++ u.userScreenName]]
        , D.li [ P.style { margin: "0px", padding: "5px" }] [case u.userProfileImageURL of
            Nothing -> D.text ""
            Just url -> D.img { src: avatarUrl url
                              , P.style {width: "100px"}] []]
        , D.li [ P.style { margin: "0px", padding: "5px" }} [case u.userDescription of
            Nothing -> D.text ""
            Just desc -> D.text desc]
        , D.li [ P.style { margin: "0px", padding: "5px" }] [case u.userURL of
            Nothing -> D.text ""
            Just url -> D.a { P.style { color: "white"
                                     , "text-decoration": "underline"}
                            , href: url
                            , target: "_blank"] [D.text url]]
        , D.li [ P.style { margin: "0px", padding: "5px" }] [case u.userLocation of
            Nothing -> D.text ""
            Just loc -> D.text loc]
        , D.li [ P.style { margin: "0px", padding: "5px" }] [case u.userTimeZone of
            Nothing -> D.text ""
            Just tz -> D.text $ tz ++ " timezone"]
        , D.li [ P.style { margin: "0px", padding: "5px" }] [
            D.text $ "Registered on " ++ u.userCreatedAt]
        , D.li [ P.style { margin: "0px", padding: "5px" }] [
            D.text $ show u.userFollowersCount ++ " followers, " ++
                        show u.userFriendsCount ++ " friends, " ++
                        show u.userStatusesCount ++ " tweets" ]
        , D.li [ P.style { margin: "0px", padding: "5px" }] [case u.userFollowing of
            Nothing -> D.text "Following status unknown"
            Just fol -> if fol
              then D.span [] [ D.text "Already following"
                             , (runEff $ unfollowButton s u) ]
              else D.span [] [ D.text "Not following"
                             , (runEff $ followButton s u) ]]
        ]

userInfo = mkUI $ spec {} \this -> do
      State { userInfo = (UserInfo { visible  = visible
                                   , userdata = mbUser } ) } <- getProps this

      pure $
          D.div [ P.className "user-info"
                , P.onDoubleClick callEventHandler stopPropagation
                , P.onClick callEventHandler stopPropagation
                , P.style { display: if visible then "block" else "none"
                          , height: "460px"
                          , width: "100%"
                          , position: "fixed"
                          , top: "50%"
                          , "overflow": "hidden"
                          , "box-shadow": "rgb(169, 169, 169) 0px 10px 50px"
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
                          , transform: "translateY(-300px)"
                          , "-webkit-transform": "translateY(-300px)"
                          } ] [

                    case mbUser of
                        Nothing -> D.img [ P.src "/snake-loader-darkbg.gif"
                                         , P.style { position: "relative"
                                                   , top: "200px" } ] []
                        Just user -> asHtml this.props.state user

                  --, D.button { P.className "writer-button nok"
                  --            , P.style { position: "absolute"
                  --                     , top: "31px"
                  --                     , transform: "translateX(240px)"
                  --                     }
                  --            , P.onClick hideUserInfo this.props.state } [D.text "⨯"]
                  ]
