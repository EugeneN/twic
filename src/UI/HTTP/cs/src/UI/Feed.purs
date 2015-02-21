module UI.Feed where

import Data.Maybe
import Data.Array
import Data.Monoid
import Data.Foldable
import Debug.Trace
import qualified Data.String as S
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Ref
import DOM (DOM(..))
import qualified React.DOM as D
import React ( createClass , eventHandler , renderComponentById , spec )
import React.Types ( Component() , ComponentClass() , Event() , React()
                   , ReactFormEvent() , ReactThis() )

import qualified Rx.Observable as Rx
import Rx.JQuery
import qualified Control.Monad.JQuery as J

import qualified Lib.WebSocket as WS
import Config (checkButtonContainerId, messagesId, containerId, socketUrl, historyUrl)
import Core
import Utils
import Types
import UI.Types
import UI.LoaderIndicator (hideLoader, showLoader)

showNewTweets :: forall eff. RefVal State
                          -> Eff (ref :: Ref, dom :: DOM, react :: React | eff) Unit
showNewTweets state = do
    State { oldFeed     = (OldFeed of_)
          , currentFeed = (CurrentFeed cf)
          , newFeed     = (NewFeed nf)
          , historyButtonDisabled = hbd
          , errors      = es } <- readState state

    writeState state $ State { oldFeed:     OldFeed $ of_ ++ cf
                             , currentFeed: CurrentFeed nf
                             , newFeed:     NewFeed []
                             , historyButtonDisabled: hbd
                             , errors:      es}

    scrollToTop
    pure unit

showOldTweets :: forall eff. RefVal State
                          -> Number
                          -> Eff (trace :: Trace, ref :: Ref, dom :: DOM, react :: React | eff) Unit
showOldTweets state count = do
    State { oldFeed     = (OldFeed of_)
          , currentFeed = (CurrentFeed cf)
          , newFeed     = (NewFeed nf)
          , historyButtonDisabled = hbd
          , errors      = es} <- readState state

    let l = length of_
        splitIdx = if l > count then (l - count) else 0
        chunks = splitAt of_ splitIdx

    case chunks of
        [newOf, historyFd] -> do
            writeState state $ State { oldFeed:     OldFeed newOf
                                     , currentFeed: CurrentFeed $ historyFd ++ cf
                                     , newFeed:     (NewFeed nf)
                                     , historyButtonDisabled: hbd
                                     , errors:      es }

            let maxid = case head newOf of
                            Just (Tweet {id_str = x}) -> x
                            Nothing -> case head historyFd of
                                Just (Tweet {id_str = y}) -> y
                                Nothing -> case head nf of
                                    Just (Tweet {id_str = z}) -> z
                                    Nothing -> "0"

            trace $ "maybe loading history w/ maxid=" ++  maxid
            maybeLoadMoreHistory state (length newOf) maxid

            pure unit

        _ -> pure unit

onError state title message = do
    State { oldFeed     = of_
          , currentFeed = cf
          , newFeed     = nf
          , historyButtonDisabled = hbd
          , errors      = es } <- readState state

    let uuid = runUUID $ getUUID

    writeState state $ State { oldFeed:     of_
                             , currentFeed: cf
                             , newFeed:     nf
                             , historyButtonDisabled: hbd
                             , errors:      es ++ [Error message uuid] }


onHistoryTweets :: forall eff. RefVal State -> [Tweet] -> Eff ( trace :: Trace, ref :: Ref | eff ) Unit
onHistoryTweets _ [] = do
    trace "got no history tweets"
    pure unit

onHistoryTweets state ts = do
    State { oldFeed =     (OldFeed of_)
          , currentFeed = cf
          , newFeed =     nf
          , historyButtonDisabled = hbd
          , errors =      es } <- readState state

    writeState state $ State { oldFeed: OldFeed $ (reverse ts) ++ of_
                             , currentFeed: cf
                             , newFeed: nf
                             , historyButtonDisabled: hbd
                             , errors: es }
    pure unit

onNewTweets :: forall eff. RefVal State -> [Tweet] -> Eff ( trace :: Trace, ref :: Ref | eff ) Unit
onNewTweets _ [] = do
    trace "got no tweets"
    pure unit

onNewTweets state ts = do
    State { oldFeed =     (OldFeed of_)
          , currentFeed = (CurrentFeed cf)
          , newFeed =     (NewFeed nf)
          , historyButtonDisabled = hbd
          , errors =      es } <- readState state

    writeState state $ State { oldFeed: OldFeed of_
                             , currentFeed: CurrentFeed cf
                             , newFeed: NewFeed $ nf ++ ts
                             , historyButtonDisabled: hbd
                             , errors: es }
    pure unit

getHistoryUrl :: TweetIdS -> Number -> String
getHistoryUrl maxid count = historyUrl ++ "?maxid=" ++ maxid ++ "&count=" ++ show count

maybeLoadMoreHistory :: forall eff. RefVal State
                                 -> Number
                                 -> TweetIdS
                                 -> Eff (trace :: Trace, ref :: Ref, react :: React, dom :: DOM | eff) Unit
maybeLoadMoreHistory state count tid | count == 0 = do
    disableHistoryButton state
    (rioGet (getHistoryUrl tid 20)) ~> handleUpdate

    where
    handleUpdate s = case (fromResponse s) of
        ResponseError {errTitle = t, errMessage = m} -> do
            enableHistoryButton state
            onError state t m
            pure unit

        ResponseSuccess {okTitle = t, okTweets = ts} -> do
            enableHistoryButton state
            -- TODO filter out item with id=`tid` from result
            onHistoryTweets state ts
            showOldTweets state 1
            pure unit

maybeLoadMoreHistory _ _ _ = pure unit

enableHistoryButton = toggleHistoryButton false
disableHistoryButton = toggleHistoryButton true

toggleHistoryButton x state = do
    State { oldFeed =     of_
          , currentFeed = cf
          , newFeed =     nf
          , historyButtonDisabled = _
          , errors =      es } <- readState state

    writeState state $ State { oldFeed:     of_
                             , currentFeed: cf
                             , newFeed:     nf
                             , historyButtonDisabled: x
                             , errors:      es }
--------------------------------------------------------------------------------

historyButton :: ComponentClass {state :: RefVal State} {}
historyButton = createClass spec { displayName = "historyButton", render = renderFun } where
    renderFun this = do
        State { historyButtonDisabled = hbd } <- readState this.props.state

        pure $
            D.button { className: if hbd then "history-button disabled" else "history-button"
                     , onClick: showOldTweets this.props.state 1
                     , "disabled": if hbd then "disabled" else ""
                     , id: "load-history-tweets-id"} [ if hbd
                        then (D.img {src: "/snake-loader.gif"} [])
                        else (D.rawText "···")]

--------------------------------------------------------------------------------

checkButton :: ComponentClass {state :: RefVal State} {}
checkButton = createClass spec { displayName = "CheckButton", render = renderFun } where
    renderFun this = do
      State { oldFeed = (OldFeed of_)
            , newFeed = (NewFeed nf)
            , currentFeed = (CurrentFeed cf)
            , errors      = es } <- readState this.props.state

      let count = length nf

      pure $
        D.button { className: if count == 0 then "no-new-tweets pop" else "there-are-new-tweets pop"
                 , onClick: showNewTweets this.props.state
                 , id: "load-new-tweets-id"} [D.rawText $ show count]

--------------------------------------------------------------------------------

-- Rx.Observable.fromEvent(document, 'wheel')
-- .bufferWithTime( 100 )
-- .filter( function (buf){ return buf.length > 5 })
-- .map( function (es){ return es.map( function(e){ return e.originalEvent.deltaY } ) } )
-- .filter(function(ys){ return ys.reduce(function(a,b){ return a + b }, 0) < 0 })
-- .subscribe(function(x){console.log('^^^', x)})


listenHistoryEvents state = obsE ~> handler
  where
  obsZ = getWheelObservable 1
  --obsZ1 = (\ev -> targetId is "load-history-tweets-id") `filterRx` obsZ
  obsA = 200 `bufferWithTime` obsZ
  obsB = (\buf -> (length buf) > 1) `filterRx` obsA
  obsC = (\es  -> getDeltaY <$> es) <$> obsB
  obsD = (\ys  -> (foldl (\a b -> a + b) 0 ys) < 0) `filterRx` obsC
  obsE = 500 `throttleWithTimeout` obsD

  handler x = do
    trace $ "got scroll up" ++ show x
    -- TODO check if document is on top
    showOldTweets state 1
    pure unit



listenFeedKeys state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"

    let keyCodesS = keyEventToKeyCode <$> bodyKeys

    (filterRx ((==) F4) keyCodesS) ~> f

    where
    f _ = do
        showNewTweets state
        pure unit

startWsClient :: forall r.  RefVal State
                         -> Eff ( react :: React
                                , ws    :: WS.WebSocket
                                , ref   :: Ref
                                , dom   :: DOM
                                , trace :: Trace | r) Unit
startWsClient state = do
    socket <- WS.mkWebSocket socketUrl

    trace "ws connected"

    WS.onMessage socket onMessage
    WS.onError   socket onError
    WS.onClose   socket onClose

    pure unit

    where
        onMessage m = onNewTweets state $ fromWsMessage m

        onError = do
            trace $ "ws error"
            pure unit

        onClose = do
            trace "ws closed"
            startWsClient state
            pure unit

-- handleRetweetClick :: forall e. String -> Eff (trace :: Trace | e) Unit
handleRetweetClick id_ = do
    let url = "/retweet/?id=" ++ id_
    (rioPost url Nothing) ~> retweetResultHandler
    pure unit

    where
        -- retweetResultHandler :: forall e. AjaxResult -> Eff (react :: React, trace :: Trace | e) Unit
        retweetResultHandler resp = do
            trace $ "retweeted " ++ show (resp :: AjaxResult)
            --renderMessage messagesId $ [Success "Retweeted :-)"]
            pure unit

-- handleStarClick :: forall e. String -> Eff (trace :: Trace | e) Unit
handleStarClick id_ = do
    let url = "/star?id=" ++ id_
    (rioPost url Nothing) ~> starResultHandler
    pure unit

    where
        -- starResultHandler :: forall e. AjaxResult -> Eff (dom :: DOM, trace :: Trace | e) Unit
        starResultHandler resp = do
            trace $ "starred " ++ show (resp :: AjaxResult)
            --renderMessage messagesId $ [Success "Starred :-)"]
            pure unit


feedClickHandler ev = do
    trace "got mouse click"
    trace $ toString ev
    pure unit

--------------------------------------------------------------------------------

processTweetElement :: Entities -> TweetElement -> TweetElement
processTweetElement (Entities {urls=urls}) (Link u) = case matchUrl u of
    Nothing -> Link u
    Just (EntityUrl {eExpandedUrl = xu}) -> Link xu

    where
        matchUrl url = case filterUrl url of
         [x] -> Just x
         _   -> Nothing

        filterUrl url = filter (f u) urls
        f u (EntityUrl {eUrl = eUrl}) = eUrl == u

processTweetElement entities x = x

getOrigTweetUrl :: Author -> String -> String
getOrigTweetUrl (Author {screen_name = screen_name}) tweetId =
    "https://twitter.com/" ++ screen_name ++ "/status/" ++ tweetId

--------------------------------------------------------------------------------

instance asHtmlTweetElement :: AsHtml TweetElement where
    asHtml (AtUsername s)   = D.span {className: "username-tag"} [
                                  D.a { href: ("https://twitter.com/" ++ s)
                                      , target: "_blank" } [D.rawText $ "@" ++ s]]

    asHtml (Link s)         = D.a { className: "inline-link"
                                  , target: "_blank", href: s} [D.rawText $ linkToText s]
        where
            linkToText u = case (S.split "/" u) !! 2 of
                Nothing -> u
                Just x  -> x

    asHtml (PlainText s)    = D.span { className: "text-tag"
                                     , dangerouslySetInnerHTML: {__html: s}} []

    asHtml (Hashtag s)      = D.span {className: "hash-tag"} [
                                D.a { href: ("https://twitter.com/hashtag/" ++ s ++ "?src=hash")
                                    , target: "_blank"}
                                    [D.rawText $ "#" ++ s]]

    asHtml (Retweet s)      = D.span {className: "retweet-tag"} [D.rawText "RT"]

    asHtml (Spaces s)       = D.span {} [D.rawText s]

    asHtml (Unparsable s)   = D.span {className: "unparsable"} [D.rawText s]

instance asHtmlTweet :: AsHtml Tweet where
    asHtml (Tweet { text = t , created_at = c , id = i , id_str = s , user = u
                  , entities = e , retweet = Nothing }) =
        tweetComponent { text: t, created_at: c, id: i, id_str: s, author: u
                       , entities: e, retweeted_by: Nothing} []

    asHtml (Tweet { created_at = c , id = i , id_str = s , user = u , retweet = Just (
                Tweet { text = origText , created_at = origCreatedAt , id = origId
                      , id_str = origIdString, entities = origEntities, user = origAuthor}) }) =
        tweetComponent { text: origText, created_at: c, id: i, id_str: s
                       , author: u, entities: origEntities, retweeted_by: Just origAuthor} []


instance asHtmlAuthor :: AsHtml Author where
    asHtml (Author {name = n, screen_name = sn, profile_image_url = avatar})
        = D.span {className: "user-icon"} [
            D.a {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                D.img {className: "user-icon-img", src: avatar, title: n} []]]

instance asHtmlEntities :: AsHtml Entities where
    asHtml (Entities { urls = us
                     , hashtags = hs
                     , media = mms })
        = case mms of
            Just ms -> D.div {className: "media"} (asHtml <$> ms)
            Nothing -> D.div {className: "media"} []

instance asHtmlMedia :: AsHtml EntityMedia where
    asHtml (EntityMedia { mType = type_
                        , mMediaUrl = url
                        })
        = case type_ of
            "photo" -> D.img {className: "inline-img", src: url} []
            x -> D.div {className: "unknown-media"} [D.rawText "Unknown media"]

--------------------------------------------------------------------------------

tweetMenu :: ComponentClass { id_str :: String
                            , author :: Author } {}
tweetMenu = createClass spec { displayName = "TweetMenu", render = renderFun }
    where
    renderFun this = pure $
        D.span {className: "toolbar-target"} [
            D.ul {className: "toolbar", id: ("menu-" ++ this.props.id_str)} [
                D.li { "data-tweet-id": (show this.props.id_str)
                     , title: "Retweet"
                     , onClick: handleRetweetClick this.props.id_str} [D.rawText "RT"]
              , D.li {} [D.a {href: (getOrigTweetUrl this.props.author this.props.id_str)
                             , target: "_blank"
                             , title: "View original"} [D.rawText "⌘"]]
              , D.li {title: "Reply"} [D.rawText "↩"]
              , D.li { title: "Star"
                     , onClick: handleStarClick this.props.id_str } [D.rawText "★"]
              ] ]

tweetComponent :: ComponentClass { text :: [TweetElement]
                                 , created_at :: String
                                 , id :: TweetId
                                 , id_str   :: String
                                 , author :: Author
                                 , entities :: Entities
                                 , retweeted_by :: Maybe Author} {}
tweetComponent = createClass spec { displayName = "Tweet" , render = renderFun }
    where
        authorToHtml a Nothing = asHtml a
        authorToHtml (Author {name = name, screen_name = sn, profile_image_url = avatar})
                        (Just ((Author {name = origName, screen_name = origSn, profile_image_url = origAvatar}))) =
            D.span {className: "user-icon"} [
                D.span {className: "user-icon2"} [
                    D.a {href: "https://twitter.com/" ++ origSn, target: "_blank"} [
                        D.img {className: "user-icon-img", src: origAvatar, title: "Original author: " ++ origName} []]]
              , D.span {className: "user-icon1"} [
                    D.a {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                        D.img {className: "user-icon-img", src: avatar, title: name} []]]
              ]

        renderFun this = pure $
          D.li {id: this.props.id_str} [ authorToHtml this.props.author this.props.retweeted_by
                  , D.span { className: "tweet-body"
                           , "data-tweet-id": this.props.id_str
                           } (asHtml <<< (processTweetElement this.props.entities) <$> this.props.text)
                  , asHtml this.props.entities ]


tweetsList :: ComponentClass {state :: RefVal State} {}
tweetsList = createClass spec { displayName = "TweetsList", render = renderFun }
    where
        renderFun this = do
          State { oldFeed = (OldFeed of_)
                      , newFeed = (NewFeed nf)
                      , currentFeed = (CurrentFeed cf)
                      , errors      = es } <- readState this.props.state

          setTitle $ case length nf of
            1 -> "1 new tweet"
            x -> (show x) ++ " new tweets"

          case cf of
            [] -> pure $ D.ul {id: "feed"
                              , onClick: (\ev -> feedClickHandler ev)} [D.li { className: "no-tweets" } [D.rawText "EOF"]]
            _  -> pure $ D.ul { id: "feed"
                              , onClick: (\ev -> feedClickHandler ev)
                              } $ asHtml <$> cf
