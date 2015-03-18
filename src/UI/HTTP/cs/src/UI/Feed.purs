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
import UI.TweetWriter (showReplyInput, showWriteInput)
import Optic.Core ( (.~), (^.))
import UI.UserInfo (loadUserInfo)
import UI.SearchInput (showSearchInput)

showNewTweets :: forall eff. RefVal State
                          -> Eff (ref :: Ref, dom :: DOM, react :: React | eff) Unit
showNewTweets state = do
    s@State { feed = AFeed { oldFeed     = (OldFeed of_)
                           , currentFeed = (CurrentFeed cf)
                           , newFeed     = (NewFeed nf) } } <- readState state

    writeState state (s # feedL .~ AFeed { oldFeed:     OldFeed $ of_ ++ cf
                                         , currentFeed: CurrentFeed nf
                                         , newFeed:     NewFeed [] })
    scrollToTop
    pure unit

showOldTweets :: forall eff. RefVal State
                          -> Number
                          -> Eff (trace :: Trace, ref :: Ref, dom :: DOM, react :: React | eff) Unit
showOldTweets state count = do
    s@State { extraFeed = ef } <- readState state

    case ef of
      Just (BFeed { oldFeed     = (OldFeed of_)
                  , currentFeed = (CurrentFeed cf)
                  , newFeed     = nf
                  , author      = author}) ->
        let l = length of_
            splitIdx = if l > count then (l - count) else 0
            chunks = splitAt of_ splitIdx

        in case chunks of
          [newOf, historyFd] ->
            writeState state $ (s # extraFeedL .~ (Just $ BFeed { oldFeed:     OldFeed newOf
                                                                , currentFeed: CurrentFeed $ historyFd ++ cf
                                                                , newFeed:     nf
                                                                , author:      author } ) )

      Nothing -> do
        s@State { feed = AFeed { oldFeed     = (OldFeed of_)
                               , currentFeed = (CurrentFeed cf)
                               , newFeed     = (NewFeed nf) } } <- readState state

        let l = length of_
            splitIdx = if l > count then (l - count) else 0
            chunks = splitAt of_ splitIdx

        case chunks of
            [newOf, historyFd] -> do
                writeState state $ (s # feedL .~ (AFeed { oldFeed:     OldFeed newOf
                                                        , currentFeed: CurrentFeed $ historyFd ++ cf
                                                        , newFeed:     (NewFeed nf) } ) )

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

onError state title message = setMessage state $ errorM message


onHistoryTweets :: forall eff. RefVal State
                            -> [Tweet]
                            -> Eff ( trace :: Trace, ref :: Ref | eff ) Unit
onHistoryTweets _ [] = do
    trace "got no history tweets"
    pure unit

onHistoryTweets state ts = do
    s <- readState state
    let feed = s ^. feedL
        newState = s # feedL .~ (feed # oldFeedL .~  ((OldFeed (reverse ts)) ++ (feed ^. oldFeedL)))
    writeState state newState

onNewTweets :: forall eff. RefVal State
                        -> [FeedMessage]
                        -> Eff ( trace :: Trace, ref :: Ref | eff ) Unit
onNewTweets _ [] = do
    trace "got no messages"
    pure unit

onNewTweets state ts = do
    s <- readState state
    let feed = s ^. feedL
        userInfo = s ^. userInfoL
        tweets = justTweets ts
        users  = justUsers ts

    handleNewTweets feed state s tweets
    handleNewUsers userInfo state s users

    pure unit

    where
    handleNewTweets feed state s xs = case xs of
        [] -> pure unit
        xs -> writeState state (s # feedL .~ (feed # newFeedL .~ ((feed ^. newFeedL) ++ NewFeed xs)))

    handleNewUsers userInfo state s ys = case ys of
        [] -> pure unit
        -- TODO check if this works
        y:_ -> writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoUserdataL .~ (Just y) ) )

getHistoryUrl :: TweetIdS -> Number -> String
getHistoryUrl maxid count = historyUrl ++ "?maxid=" ++ maxid ++ "&count=" ++ show count

maybeLoadMoreHistory :: forall eff. RefVal State
                                 -> Number
                                 -> TweetIdS
                                 -> Eff (trace :: Trace, ref :: Ref, react :: React, dom :: DOM | eff) Unit
maybeLoadMoreHistory state count tid | count == 0 = do
    disableHistoryButton state
    (rioGet (getHistoryUrl tid 20)) ~> handler

    where
    handler s = case (fromResponse s) of
        ResponseError {errTitle = t, errMessage = m} -> do
            enableHistoryButton state
            onError state t m
            pure unit

        ResponseSuccess {okTitle = t, okFeedMessages = ts} -> do
            enableHistoryButton state
            -- TODO filter out item with id=`tid` from result
            onHistoryTweets state $ justTweets ts
            showOldTweets state 1
            pure unit

maybeLoadMoreHistory _ _ _ = pure unit



--------------------------------------------------------------------------------

historyButtonContextMenuHandler state ev = do
    stopPropagation ev
    resetContextMenu state
    showSearchInput state

historyButton :: ComponentClass {state :: RefVal State} {}
historyButton = createClass spec { displayName = "historyButton", render = renderFun }
    where
    renderFun this = do
        State { historyButtonDisabled = hbd } <- readState this.props.state

        pure $
            D.button { className: if hbd then "history-button disabled"
                                         else "history-button"
                     , onClick: showOldTweets this.props.state 1
                     , onContextMenu: (callEventHandler $ historyButtonContextMenuHandler this.props.state)
                     , "disabled": if hbd then "disabled" else ""
                     , id: "load-history-tweets-id"} [ if hbd
                        then (D.img {src: "/snake-loader.gif"} [])
                        else (D.rawText "···")]

--------------------------------------------------------------------------------

checkButtonContextMenuHandler state ev = do
    stopPropagation ev
    resetContextMenu state
    showWriteInput state

checkButton :: ComponentClass {state :: RefVal State} {}
checkButton = createClass spec { displayName = "CheckButton", render = renderFun }
    where
    renderFun this = do
      State { feed = AFeed { newFeed = (NewFeed nf) } } <- readState this.props.state

      let count = length nf

      pure $
        D.button { className: if count == 0 then "no-new-tweets pop"
                                            else "there-are-new-tweets pop"
                 , onClick: showNewTweets this.props.state
                 , onContextMenu: (callEventHandler $ checkButtonContextMenuHandler this.props.state)
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

handleRetweetClick state id_ = do
    resetContextMenu state
    let url = "/retweet/?id=" ++ id_
    disableHistoryButton state
    (rioPost url Nothing) ~> retweetResultHandler
    pure unit

    where
    retweetResultHandler resp = do
        enableHistoryButton state
        trace $ "retweeted " ++ show (resp :: AjaxResult)
        setMessage state (successM "Retweeted :-)")
        pure unit

handleStarClick state id_ = do
    resetContextMenu state
    let url = "/star?id=" ++ id_
    disableHistoryButton state
    (rioPost url Nothing) ~> starResultHandler
    pure unit

    where
    starResultHandler resp = do
        enableHistoryButton state
        trace $ "starred " ++ show (resp :: AjaxResult)
        setMessage state (successM "Starred :-)")
        pure unit

handleAuthorContextMenu state author@(Author {screen_name=sn}) ev = do
  stopPropagation ev
  resetContextMenu state
  loadUserInfo state author

getTweetById :: forall eff. RefVal State
                         -> TweetIdS
                         -> Eff (ref :: Ref | eff) (Maybe Tweet)
getTweetById state tid = do
    s <- readState state
    let cf = (s ^. feedL) ^. currentFeedL -- TODO make extra feed work too
        unp (CurrentFeed ts) = ts
        res = filter (\(Tweet {id_str = cur_id}) -> cur_id == tid) (unp cf)

    return $ case res of
        []    -> Nothing
        [x]   -> Just x
        x:xs  -> Just x

handleReplyClick state id_ = do
    resetContextMenu state
    x <- getTweetById state id_
    case x of
        Nothing    -> setMessage state (errorM $ "Cant't find tweet with id=" ++ id_)
        Just tweet -> showReplyInput state x

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

loadUserFeedByName state sn =
    loadUserFeed state (Author { name: sn -- TODO FIXME
                               , authorId: 0
                               , screen_name: sn
                               , default_profile_image: true
                               , profile_image_url: "?" })

loadUserFeed state author@(Author {screen_name = sn}) = do
    trace "gonna load user feed"
    let url = "/userfeed/?sn=" ++ sn
    disableHistoryButton state
    (rioGet url) ~> handler

    where
    getAuthor (Tweet { user = x }) = x

    handler resp = case (fromResponse resp) of
        ResponseError {errTitle = t, errMessage = m} -> do
            enableHistoryButton state
            onError state t m
            pure unit

        ResponseSuccess {okTitle = t, okFeedMessages = xs} -> do
            enableHistoryButton state
            s <- readState state

            let ts = justTweets xs

            case ts of
              x:xs -> writeState state (s # extraFeedL .~ (Just $ BFeed { oldFeed: OldFeed (reverse xs)
                                                                        , currentFeed: CurrentFeed [x]
                                                                        , newFeed: NewFeed []
                                                                        , author: Just (getAuthor x) }))

              [x]  -> writeState state (s # extraFeedL .~ (Just $ BFeed { oldFeed: OldFeed []
                                                                        , currentFeed: CurrentFeed [x]
                                                                        , newFeed: NewFeed []
                                                                        , author: Just (getAuthor x) }))

              -- XXX how to get Author if we're called by sn and the user has no tweets at all?
              _    -> writeState state (s # extraFeedL .~ (Just $ BFeed { oldFeed: OldFeed []
                                                                        , currentFeed: CurrentFeed []
                                                                        , newFeed: NewFeed []
                                                                        , author: Just author }))

--------------------------------------------------------------------------------

instance asHtmlTweetElement :: AsHtml TweetElement where
    asHtml s (AtUsername sn)   = D.span {className: "username-tag"} [
                                  D.span { href: ("https://twitter.com/" ++ sn)
                                         , onClick: loadUserFeedByName s sn
                                         --, onContextMenu: (callEventHandler $ handleAuthorContextMenu state a)
                                         , style: { "cursor": "pointer"
                                                  , "color": "black" }
                                         , target: "_blank" } [D.rawText $ "@" ++ sn]]

    asHtml _ (Link s)         = D.a { className: "inline-link"
                                  , target: "_blank", href: s} [D.rawText $ linkToText s]
        where
            linkToText u = case (S.split "/" u) !! 2 of
                Nothing -> u
                Just x  -> x

    asHtml _ (PlainText s)    = D.span { className: "text-tag"
                                     , dangerouslySetInnerHTML: {__html: s}} []

    asHtml _ (Hashtag s)      = D.span {className: "hash-tag"} [
                                D.a { href: ("https://twitter.com/hashtag/" ++ s ++ "?src=hash")
                                    , target: "_blank"}
                                    [D.rawText $ "#" ++ s]]

    asHtml _ (Retweet s)      = D.span {className: "retweet-tag"} [D.rawText "RT"]

    asHtml _ (Spaces s)       = D.span {} [D.rawText s]

    asHtml _ (Unparsable s)   = D.span {className: "unparsable"} [D.rawText s]

instance asHtmlTweet :: AsHtml Tweet where
    asHtml state (Tweet { text = t , created_at = c , id = i , id_str = s , user = u
                  , entities = e , retweet = Nothing }) =
        tweetComponent { state: state, text: t, created_at: c, id: i, id_str: s, author: u
                       , entities: e, retweeted_by: Nothing } []

    asHtml state (Tweet {  created_at = c , id = i , id_str = s , user = u , retweet = Just (
                Tweet { text = origText , created_at = origCreatedAt , id = origId
                      , id_str = origIdString, entities = origEntities, user = origAuthor}) }) =
        tweetComponent { state: state, text: origText, created_at: c, id: i, id_str: s
                       , author: u, entities: origEntities, retweeted_by: Just origAuthor} []


instance asHtmlAuthor :: AsHtml Author where
    asHtml state a@(Author {name = n, screen_name = sn, profile_image_url = avatar})
        = D.span {className: "user-icon"} [
            D.span {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                D.img { className: "user-icon-img"
                      , style: { "cursor": "pointer" }
                      , onClick: loadUserFeed state a
                      , onContextMenu: (callEventHandler $ handleAuthorContextMenu state a)
                      , src: avatar, title: n} []]]

instance asHtmlEntities :: AsHtml Entities where
    asHtml state (Entities { urls = us
                     , hashtags = hs
                     , media = mms })
        = case mms of
            Just ms -> D.div {className: "media"} ((asHtml state) <$> ms)
            Nothing -> D.div {className: "media"} []

instance asHtmlMedia :: AsHtml EntityMedia where
    asHtml state (EntityMedia { mType = type_
                        , mMediaUrl = url
                        })
        = case type_ of
            "photo" -> D.img {className: "inline-img", src: url} []
            x -> D.div {className: "unknown-media"} [D.rawText "Unknown media"]

--------------------------------------------------------------------------------

tweetMenu :: ComponentClass { state :: RefVal State } {}
tweetMenu = createClass spec { displayName = "TweetMenu", render = renderFun }
    where
    renderFun this = do
      State { contextMenu = (ContextMenu { visible = visible
                                         , x = x
                                         , y = y
                                         , tweetId = maybeTid
                                         }) } <- readState this.props.state

      pure $ case maybeTid of
        Just tid -> D.span { className: "toolbar-target"
                           , onContextMenu: callEventHandler stopPropagation
                           , onClick: callEventHandler stopPropagation
                           , style: { display: if visible then "block" else "none"
                                    , position: "absolute"
                                    , left: x
                                    , top: y
                                    } } [
                        D.ul {className: "toolbar", id: ("menu-" ++ tid)} [
                            D.li { "data-tweet-id": tid
                                 , title: "Retweet"
                                 , onClick: handleRetweetClick this.props.state tid}
                              [D.rawText "RT"]

                          , D.li { title: "Reply"
                                 , onClick: handleReplyClick this.props.state tid }
                              [D.rawText "↩"]

                          , D.li { title: "Star"
                                 , onClick: handleStarClick this.props.state tid }
                              [D.rawText "★"]

                          , D.li {}
                              [D.a {href: (getOrigTweetUrl (Author { name: "fake"
                                                                   , authorId: 0
                                                                   , screen_name: "this.props.author"
                                                                   , default_profile_image: true
                                                                   , profile_image_url: "-"}) tid)
                                                                   , target: "_blank"
                                                                   , title: "View original"}
                                 [D.rawText "⌘"]]
                          ] ]

        Nothing -> D.span {className: "toolbar-target"} [ D.rawText "No tweet selected" ]



showContextMenu state x y tid = do
    s <- readState state
    writeState state (s # contextMenuL .~ ContextMenu { visible: true
                                                      , x: x
                                                      , y: y
                                                      , tweetId: Just tid })

tweetContextMenu state id_str e = do
    stopPropagation e
    showContextMenu state
                    e.nativeEvent.pageX
                    e.nativeEvent.pageY
                    id_str

tweetComponent :: ComponentClass { state    :: RefVal State
                                 , text     :: [TweetElement]
                                 , created_at :: String
                                 , id       :: TweetId
                                 , id_str   :: String
                                 , author   :: Author
                                 , entities :: Entities
                                 , retweeted_by :: Maybe Author} {}
tweetComponent = createClass spec { displayName = "Tweet" , render = renderFun }
    where
    authorToHtml state a Nothing = asHtml state a
    authorToHtml state a@(Author { name = name, screen_name = sn
                                 , profile_image_url = avatar})
                    (Just (b@(Author { name = origName, screen_name = origSn
                                     , profile_image_url = origAvatar}))) =
        D.span {className: "user-icon"} [
            D.span {className: "user-icon2"} [
                D.span {href: "https://twitter.com/" ++ origSn, target: "_blank"} [
                    D.img { className: "user-icon-img"
                          , onClick: loadUserFeed state b
                          , style: { "cursor": "pointer" }
                          , src: origAvatar
                          , onContextMenu: (callEventHandler $ handleAuthorContextMenu state b)
                          , title: "Original author: " ++ origName} []]]
          , D.span {className: "user-icon1"} [
                D.span {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                    D.img { className: "user-icon-img"
                          , onClick: loadUserFeed state a
                          , style: { "cursor": "pointer" }
                          , src: avatar
                          , onContextMenu: (callEventHandler $ handleAuthorContextMenu state a)
                          , title: name} []]]
          ]

    renderFun this = pure $
        D.li {id: this.props.id_str} [
            authorToHtml this.props.state this.props.author this.props.retweeted_by
          , D.span { className: "tweet-body"
                   , "data-tweet-id": this.props.id_str
                   , onContextMenu: (callEventHandler $ tweetContextMenu this.props.state this.props.id_str)
                   } ((asHtml this.props.state) <<< (processTweetElement this.props.entities) <$> this.props.text)
          , asHtml this.props.state this.props.entities ]


tweetsList :: ComponentClass {state :: RefVal State} {}
tweetsList = createClass spec { displayName = "TweetsList", render = renderFun }
    where
    renderFun this = do
        State { feed = AFeed { newFeed = (NewFeed nf)
                             , currentFeed = (CurrentFeed cf) }
              , extraFeed = maybeBFeed } <- readState this.props.state

        case maybeBFeed of
            Nothing -> do
                setTitle $ case length nf of
                    1 -> "1 new tweet"
                    x -> (show x) ++ " new tweets"

                case cf of
                    [] -> pure $ D.ul {id: "feed" , onClick: (\ev -> feedClickHandler ev)}
                            [D.li { className: "no-tweets" } [D.rawText "EOF"]]

                    _  -> pure $ D.ul {id: "feed" , onClick: (\ev -> feedClickHandler ev) }
                            $ (asHtml this.props.state) <$> cf

            Just (BFeed { author = mbAuthor
                        , currentFeed = (CurrentFeed cf)
                        , newFeed = (NewFeed nf) }) -> case mbAuthor of

                Nothing -> do
                    setTitle "Maybe Search results"
                    case cf of
                        [] -> pure $ D.ul {id: "feed" , onClick: (\ev -> feedClickHandler ev)}
                                [D.li { className: "no-tweets" } [D.rawText "EOF"]]

                        _  -> pure $ D.ul {id: "feed" , onClick: (\ev -> feedClickHandler ev) }
                                $ (asHtml this.props.state) <$> cf

                Just (Author {screen_name = sn}) -> do
                    setTitle $ sn ++ "'s tweets"
                    case cf of
                        [] -> pure $ D.ul {id: "feed", onClick: (\ev -> feedClickHandler ev)}
                                [D.li {className: "no-tweets" } [D.rawText "EOF"]]

                        _  -> pure $ D.ul {id: "feed", onClick: (\ev -> feedClickHandler ev)}
                                $ (asHtml this.props.state) <$> cf
