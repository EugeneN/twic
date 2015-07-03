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
import qualified Data.Set as DS

showNewTweets :: forall eff. Ref State
                          -> Eff (ref :: REF, dom :: DOM, react :: React | eff) Unit
showNewTweets state = do
    s@State { feed = AFeed { oldFeed     = (OldFeed of_)
                           , currentFeed = (CurrentFeed cf)
                           , newFeed     = (NewFeed nf) } } <- readState state

    writeState state (s # feedL .~ AFeed { oldFeed:     OldFeed $ of_ ++ cf
                                         , currentFeed: CurrentFeed nf
                                         , newFeed:     NewFeed [] })
    scrollToTop
    pure unit

showOldTweets :: forall eff. Ref State
                          -> Number
                          -> Eff (trace :: Trace, ref :: REF, dom :: DOM, react :: React | eff) Unit
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


onHistoryTweets :: forall eff. Ref State
                            -> Array Tweet
                            -> Eff ( trace :: Trace, ref :: REF | eff ) Unit
onHistoryTweets _ [] = do
    trace "got no history tweets"
    pure unit

onHistoryTweets state ts = do
    s <- readState state
    let feed = s ^. feedL
        newState = s # feedL .~ (feed # oldFeedL .~  ((OldFeed (reverse ts)) ++ (feed ^. oldFeedL)))
    writeState state newState

onNewFeedMessages :: forall eff. Ref State
                              -> Array FeedMessage
                              -> Eff ( trace :: Trace, ref :: REF | eff ) Unit
onNewFeedMessages _ [] = do
    trace "got no messages"
    pure unit

onNewFeedMessages state ts = do
    s <- readState state

    handleNewTweets state s (justTweets ts)
    handleNewUsers  state s (justUsers ts)
    handleErrors    state   (justErrors ts)
    handleAccount   state s (justSettings ts) (justFriendsLists ts)

    pure unit

    where
    setError (ResponseError {errMessage = msg}) = setMessage state $ errorM msg
    handleErrors state errors_ = do
        let z = setError <$> errors_
        pure unit

    -- TODO merge settings?
    mergeFriends :: Array User -> Maybe (DS.Set User) -> DS.Set User
    mergeFriends newfriends oldfriends =
        let newFriends = DS.fromList newfriends
            oldFriends = case oldfriends of
                Nothing -> DS.empty
                Just fs -> fs

        in newFriends `DS.union` oldFriends

    handleAccount state s [] [] = pure unit
    handleAccount state s settingss [] =
        writeState state (s # accountL .~ ((s ^. accountL) # accountSettingsL .~ Just (settingss !! 0)))

    handleAccount state s [] friends =
        writeState state (s # accountL .~
            ((s ^. accountL) # accountFriendsL .~
                Just (friends `mergeFriends` (s ^. accountL ^. accountFriendsL) )))

    handleAccount state s settingss friends =
        writeState state (s # accountL .~ Account { settings: Just (settings !! 0)
                                                  , friends: Just (friends `mergeFriends` (s ^. accountL ^. accountFriendsL) ) })

    handleNewTweets :: forall eff. Ref State -> State -> Array Tweet -> Eff (ref :: REF | eff) Unit
    handleNewTweets state s xs = case xs of
        [] -> pure unit
        xs -> writeState state (s # feedL .~ (feed # newFeedL .~ (cleanNewFeed rawNewFeed)))
            where
            feed = s ^. feedL
            rawNewFeed = (feed ^. newFeedL) ++ NewFeed xs
            cleanNewFeed (NewFeed xs) = NewFeed $ nub xs


    handleNewUsers state s ys = case ys of
        [] -> pure unit
        -- TODO check if this works
        ys -> writeState state (s # userInfoL .~ ( (s ^. userInfoL) # userInfoUserdataL .~ (Just (ys !! 0)) ) )

getHistoryUrl :: TweetIdS -> Number -> String
getHistoryUrl maxid count = historyUrl ++ "?maxid=" ++ maxid ++ "&count=" ++ show count

maybeLoadMoreHistory :: forall eff. Ref State
                                 -> Number
                                 -> TweetIdS
                                 -> Eff (trace :: Trace, ref :: REF, react :: React, dom :: DOM | eff) Unit
maybeLoadMoreHistory state count tid | count == 0 = do
    disableHistoryButton state
    (rioGet (getHistoryUrl tid 20)) `Rx.subscribe` handler

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

historyButtonDoubleClickHandler state ev = do
    stopPropagation ev
    resetContextMenu state
    showSearchInput state

historyButton :: ComponentClass {state :: REFVal State} {}
historyButton = mkUI $ spec {} 	his -> do
    where
    renderFun this = do
        State { historyButtonDisabled = hbd } <- readState this.props.state

        pure $
            D.button [ P.className $ if hbd then "history-button disabled"
                                            else "history-button"
                     , P.onClick (showOldTweets this.props.state 1)
                     , P.onDoubleClick (callEventHandler $ historyButtonDoubleClickHandler this.props.state)
                     , P.disabled $ if hbd then "disabled" else ""
                     , P.id "load-history-tweets-id" ] [
                        if hbd then (D.img [ P.src "/snake-loader.gif" ] [])
                               else (D.text "···")]

--------------------------------------------------------------------------------

checkButtonDoubleClickHandler state ev = do
    stopPropagation ev
    resetContextMenu state
    showWriteInput state

checkButton :: ComponentClass {state :: REFVal State} {}
checkButton = mkUI $ spec {} 	his -> do
    where
    renderFun this = do
      State { feed = AFeed { newFeed = (NewFeed nf) } } <- readState this.props.state

      let count = length nf

      pure $
        D.button [ P.className if count == 0 then "no-new-tweets pop"
                                             else "there-are-new-tweets pop"
                 , P.onClick showNewTweets this.props.state
                 , P.onDoubleClick: (callEventHandler $ checkButtonDoubleClickHandler this.props.state)
                 , P.id: "load-new-tweets-id" ] [D.text $ show count]

--------------------------------------------------------------------------------

-- Rx.Observable.fromEvent(document, 'wheel')
-- .bufferWithTime( 100 )
-- .filter( function (buf){ return buf.length > 5 })
-- .map( function (es){ return es.map( function(e){ return e.originalEvent.deltaY } ) } )
-- .filter(function(ys){ return ys.reduce(function(a,b){ return a + b }, 0) < 0 })
-- .subscribe(function(x){console.log('^^^', x)})

listenHomeEvents state = do
    bodyKeys <- J.select "body" >>= onAsObservable "keyup"
    (filterRx ((==) Home) (keyEventToKeyCode <$> bodyKeys)) `Rx.subscribe` handler state

    where
    handler state _ =  do
        s <- readState state
        case s ^. accountL ^. accountSettingsL of
            Just (TASettings { accScreenName = sn }) -> loadUserFeedByName state sn
            Nothing -> onError state "No account info available" "No account info available"

listenHistoryEvents state = obsE `Rx.subscribe` handler
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

    (filterRx ((==) F4) keyCodesS) `Rx.subscribe` f

    where
    f _ = do
        showNewTweets state
        pure unit

startWsClient :: forall r.  Ref State
                         -> Eff ( react :: React
                                , ws    :: WS.WebSocket
                                , ref   :: REF
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
    onMessage m = onNewFeedMessages state $ fromWsMessage m

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
    (rioPost url Nothing) `Rx.subscribe` retweetResultHandler
    retweetInplace state id_
    pure unit

    where
    retweetResultHandler resp = do
        enableHistoryButton state
        trace $ "retweeted " ++ show (resp :: AjaxResult)
        setMessage state (successM "Retweeted :-)")
        pure unit

handleUnretweetClick state id_ = do
    resetContextMenu state
    let url = "/unretweet/?id=" ++ id_
    disableHistoryButton state
    (rioPost url Nothing) `Rx.subscribe` unretweetResultHandler
    unretweetInplace state id_
    pure unit

    where
    unretweetResultHandler resp = do
        enableHistoryButton state
        trace $ "unretweeted " ++ show (resp :: AjaxResult)
        setMessage state (successM "Unretweeted :-)")
        pure unit

handleUnstarClick state id_ = do
    resetContextMenu state
    let url = "/unstar?id=" ++ id_
    disableHistoryButton state
    (rioPost url Nothing) `Rx.subscribe` starResultHandler
    unstarInplace state id_
    pure unit

    where
    starResultHandler resp = do
        enableHistoryButton state
        trace $ "unstarred " ++ show (resp :: AjaxResult)
        setMessage state (successM "Unstarred :-)")
        pure unit

handleOrigLink state url = do
    resetContextMenu state
    openInNewWindow url
    pure unit

handleStarClick state id_ = do
    resetContextMenu state
    let url = "/star?id=" ++ id_
    disableHistoryButton state
    (rioPost url Nothing) `Rx.subscribe` starResultHandler
    starInplace state id_
    pure unit

    where
    starResultHandler resp = do
        enableHistoryButton state
        trace $ "starred " ++ show (resp :: AjaxResult)
        setMessage state (successM "Starred :-)")
        pure unit

retweetInplace state tid = modifyCurrentFeedInplace state mapRetweet
  where
  mapRetweet t@(Tweet {id_str=cid}) = if tid == cid
    then t # retweetedL .~ true
    else t

unretweetInplace state tid = modifyCurrentFeedInplace state mapUnretweet
  where
  mapUnretweet t@(Tweet {id_str=cid}) = if tid == cid
    then t # retweetedL .~ false
    else t

starInplace state tid = modifyCurrentFeedInplace state mapStar
  where
  mapStar t@(Tweet {id_str=cid}) = if tid == cid
    then t # favoritedL .~ true
    else t

unstarInplace state tid = modifyCurrentFeedInplace state mapUnstar
  where
  mapUnstar t@(Tweet {id_str=cid}) = if tid == cid
    then t # favoritedL .~ false
    else t

modifyCurrentFeedInplace state mutator = do
  s@(State {feed = f@(AFeed {currentFeed = cf})}) <- readState state
  writeState state (s # feedL .~ (f # currentFeedL .~ (mutator <$> cf)))

handleAuthorContextMenu state author@(Author {screen_name=sn}) ev = do
  stopPropagation ev
  resetContextMenu state
  loadUserInfo state author

getTweetById :: forall eff. Ref State
                         -> TweetIdS
                         -> Eff (ref :: REF | eff) (Maybe Tweet)
getTweetById state tid = do
    s <- readState state
    return $ getTweetById' s tid


getTweetById' ::  State -> TweetIdS -> Maybe Tweet
getTweetById' s tid =
    let cf = (s ^. feedL) ^. currentFeedL -- TODO make extra feed work too
        unp (CurrentFeed ts) = ts
        res = filter (\(Tweet {id_str = cur_id}) -> cur_id == tid) (unp cf)

    in case res of
        []    -> Nothing
        [x]   -> Just x
        xs    -> Just (x !! 0)

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
    (rioGet url) `Rx.subscribe` handler

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
              xs   -> writeState state (s # extraFeedL .~ (Just $ BFeed { oldFeed: OldFeed (reverse (tail xs))
                                                                        , currentFeed: CurrentFeed [(head xs)]
                                                                        , newFeed: NewFeed []
                                                                        , author: Just (getAuthor (head xs)) }))

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
    asHtml s (AtUsername sn)   = D.span [ P.className "username-tag"} [
                                      D.span { P.href ("https://twitter.com/" ++ sn)
                                             , P.onClick $ loadUserFeedByName s sn
                                             --, P.onDoubleClick: (callEventHandler $ handleAuthorContextMenu state a)
                                             , P.style { "cursor": "pointer"
                                                       , "color": "black" }
                                             , P.target "_blank" } [D.text $ "@" ++ sn]]

    asHtml _ (Link s)         = D.a [ P.className "inline-link"
                                    , P.target "_blank", href: s ] [D.text $ linkToText s]
        where linkToText u = case (S.split "/" u) !! 2 of
                                Nothing -> u
                                Just x  -> x

    asHtml _ (PlainText s)    = D.span [ P.className "text-tag"
                                       , P.dangerouslySetInnerHTML {__html: s}] []

    asHtml _ (Hashtag s)      = D.span [ P.className "hash-tag"} [
                                    D.a [ P.href ("https://twitter.com/hashtag/" ++ s ++ "?src=hash")
                                        , P.target "_blank" ] [D.text $ "#" ++ s]]

    asHtml _ (Retweet s)      = D.span [ P.className "retweet-tag" ] [D.text "RT"]

    asHtml _ (Spaces s)       = D.span [] [D.text s]

    asHtml _ (Unparsable s)   = D.span [ P.className "unparsable" ] [D.text s]

instance asHtmlTweet :: AsHtml Tweet where
    asHtml state (Tweet { text = t , created_at = c , id = i , id_str = s , user = u
                  , entities = e , retweet = Nothing, favorited=favorited, retweeted=retweeted }) =
        tweetComponent { state: state, text: t, created_at: c, id: i, id_str: s, author: u
                       , entities: e, retweeted_by: Nothing, favorited: favorited, retweeted: retweeted } []

    asHtml state (Tweet {  created_at = c , id = i , id_str = s , user = u , retweet = Just (
                Tweet { text = origText , created_at = origCreatedAt , id = origId
                      , id_str = origIdString, entities = origEntities, user = origAuthor, favorited=favorited, retweeted=retweeted}) }) =
        tweetComponent { state: state, text: origText, created_at: c, id: i, id_str: s
                       , author: u, entities: origEntities, retweeted_by: Just origAuthor, favorited: favorited, retweeted: retweeted} []


instance asHtmlAuthor :: AsHtml Author where
    asHtml state a@(Author {name = n, screen_name = sn, profile_image_url = avatar})
        = D.span [ P.className "user-icon"} [
            D.span [ P.href "https://twitter.com/" ++ sn
                   , target: "_blank" ] [ D.img [ P.className "user-icon-img"
                                                , P.style { "cursor": "pointer" }
                                                , P.onClick $ loadUserFeed state a
                                                , P.onDoubleClick (callEventHandler $ handleAuthorContextMenu state a)
                                                , P.src avatar
                                                , P.title n ] []]]

instance asHtmlEntities :: AsHtml Entities where
    asHtml state (Entities { urls = us
                     , hashtags = hs
                     , media = mms })
        = case mms of
            Just ms -> D.div [ P.className "media" ] ((asHtml state) <$> ms)
            Nothing -> D.div [ P.className "media" ] []

instance asHtmlMedia :: AsHtml EntityMedia where
    asHtml state (EntityMedia { mType = type_
                              , mMediaUrl = url })
        = case type_ of
            "photo" -> D.a [ P.href url, P.target "_blank" ] [
                        D.img [ P.className "inline-img", P.src url ] []]
            x -> D.div [ P.className "unknown-media" ] [D.text "Unknown media"]

--------------------------------------------------------------------------------

tweetMenu = mkUI $ spec {} \this -> do
      s@(State { contextMenu = (ContextMenu { visible = visible
                                            , x = x
                                            , y = y
                                            , tweetId = maybeTid
                                            }) }) <- readState this.props.state

      pure $ case maybeTid of
        Just tid ->
            let mbTweet = getTweetById' s tid
            in D.span [ P.className "toolbar-target"
                      , P.onDoubleClick $ callEventHandler stopPropagation
                      , P.onClick $ callEventHandler stopPropagation
                      , P.style { display: if visible then "block" else "none"
                                , position: "absolute"
                                , left: x
                                , top: y
                                } ] [

                    case mbTweet of
                        Nothing ->  D.span [ P.className "toolbar-target" ] [ D.text "No tweet found o_O" ]

                        Just (Tweet { favorited = favorited
                                    , retweeted = retweeted}) ->
                            D.ul [ P.className "toolbar", P.id ("menu-" ++ tid) ] [
                                if retweeted
                                    then
                                        D.li [ P.title "Un-retweet"
                                             , P.onClick $ handleUnretweetClick this.props.state tid ]
                                          [D.text "♻"]
                                    else
                                        D.li [ P.title "Retweet"
                                             , P.onClick $ handleRetweetClick this.props.state tid ]
                                          [D.text "♲"]

                              , D.li [ P.title "Reply"
                                     , P.onClick $ handleReplyClick this.props.state tid ]
                                  [D.text "↩"]

                              , if favorited
                                    then
                                        D.li [ P.title "Unstar"
                                             , P.onClick $ handleUnstarClick this.props.state tid ]
                                          [D.text "♥"]
                                    else
                                        D.li [ P.title "Star"
                                             , P.onClick $ handleStarClick this.props.state tid ]
                                          [D.text "♡"]

                              , D.li [ P.title "View original"
                                     , P.onClick $ handleOrigLink this.props.state (getOrigTweetUrl (Author { name: "fake"
                                                                                                            , authorId: 0
                                                                                                            , screen_name: "this.props.author"
                                                                                                            , default_profile_image: true
                                                                                                            , profile_image_url: "-"}) tid) ] [D.text "⌘"]
                              ] ]

        Nothing -> D.span [ P.className "toolbar-target" ] [ D.text "No tweet selected" ]



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

tweetComponent :: ComponentClass { state        :: REFVal State
                                 , text         :: Attay TweetElement
                                 , created_at   :: String
                                 , id           :: TweetId
                                 , id_str       :: String
                                 , author       :: Author
                                 , entities     :: Entities
                                 , retweeted_by :: Maybe Author
                                 , retweeted    :: Boolean
                                 , favorited    :: Boolean } {}
tweetComponent = mkUI $ spec {} \this -> do
    where
    authorToHtml state a Nothing = asHtml state a
    authorToHtml state a@(Author { name = name, screen_name = sn
                                 , profile_image_url = avatar})
                    (Just (b@(Author { name = origName, screen_name = origSn
                                     , profile_image_url = origAvatar}))) =
        D.span [ P.className "user-icon" ] [
            D.span [ P.className "user-icon2" ] [
                D.span [ P.href "https://twitter.com/" ++ origSn, P.target "_blank" ] [
                    D.img [ P.className "user-icon-img"
                          , P.onClick loadUserFeed state b
                          , P.style { "cursor": "pointer" }
                          , P.src origAvatar
                          , P.onDoubleClick (callEventHandler $ handleAuthorContextMenu state b)
                          , P.title $ "Original author: " ++ origName ] []]]
          , D.span [ P.className "user-icon1"} [
                D.span [ P.href "https://twitter.com/" ++ sn
                       , P.target "_blank" ] [
                    D.img [ P.className "user-icon-img"
                          , P.onClick loadUserFeed state a
                          , P.style { "cursor": "pointer" }
                          , P.src avatar
                          , P.onDoubleClick (callEventHandler $ handleAuthorContextMenu state a)
                          , P.title name ] []]]
          ]

    bgColor fav retw | fav  &&      retw  = "rgb(216, 241, 251)"
    bgColor fav retw | fav  && (not retw) = "yellow"
    bgColor fav retw | retw && (not fav)  = "rgb(213, 255, 213)"
    bgColor _ _                           = "transparent"

    renderFun this = pure $
        D.li { id: this.props.id_str
             } [
            authorToHtml this.props.state this.props.author this.props.retweeted_by
          , D.span { P.className "tweet-body"
                   , P.style {"background-color": bgColor this.props.favorited this.props.retweeted}
                   , "data-tweet-id": this.props.id_str
                   , P.onDoubleClick: (callEventHandler $ tweetContextMenu this.props.state this.props.id_str)
                   } ((asHtml this.props.state) <<< (processTweetElement this.props.entities) <$> this.props.text)
          , asHtml this.props.state this.props.entities ]


tweetsList :: ComponentClass {state :: REFVal State} {}
tweetsList = mkUI $ spec {} 	his -> do
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
                    [] -> pure $ D.ul {id: "feed" , P.onClick (\ev -> feedClickHandler ev)}
                            [D.li { P.className "no-tweets" } [D.text "EOF"]]

                    _  -> pure $ D.ul {id: "feed" , P.onClick (\ev -> feedClickHandler ev) }
                            $ (asHtml this.props.state) <$> cf

            Just (BFeed { author = mbAuthor
                        , currentFeed = (CurrentFeed cf)
                        , newFeed = (NewFeed nf) }) -> case mbAuthor of

                Nothing -> do
                    setTitle "Maybe Search results"
                    case cf of
                        [] -> pure $ D.ul {id: "feed" , P.onClick (\ev -> feedClickHandler ev)}
                                [D.li { P.className "no-tweets" } [D.text "EOF"]]

                        _  -> pure $ D.ul {id: "feed" , P.onClick (\ev -> feedClickHandler ev) }
                                $ (asHtml this.props.state) <$> cf

                Just (Author {screen_name = sn}) -> do
                    setTitle $ sn ++ "'s tweets"
                    case cf of
                        [] -> pure $ D.ul {id: "feed", P.onClick (\ev -> feedClickHandler ev)}
                                [D.li [ P.className "no-tweets" } [D.text "EOF"]]

                        _  -> pure $ D.ul {id: "feed", P.onClick (\ev -> feedClickHandler ev)}
                                $ (asHtml this.props.state) <$> cf
