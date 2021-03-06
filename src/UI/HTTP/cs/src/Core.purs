module Core where

import Control.Monad.Eff (Eff(..))
import Data.Foreign
import Data.Foreign.Index (hasOwnProperty)
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import DOM (DOM(..))
import Control.Monad.Eff.Ref
import Prelude
import Data.Foreign
import Data.Foreign.Class
import Data.Either
import Data.Monoid
import Data.Maybe
import Optic.Core
import Data.Array (filter, concat)

import Utils
import Types
import Config
import Data.Set


toTweetToken :: String -> String -> TweetElement
toTweetToken "AtUsername"   = AtUsername
toTweetToken "Link"         = Link
toTweetToken "PlainText"    = PlainText
toTweetToken "Hashtag"      = Hashtag
toTweetToken "Spaces"       = Spaces
toTweetToken "Unparsable"   = Unparsable

instance isForeignUser :: IsForeign User where
    read rawObj = do
         userContributorsEnabled              <- readProp "contributors_enabled" rawObj
         userCreatedAt                        <- readProp "created_at" rawObj
         userDefaultProfile                   <- readProp "default_profile" rawObj
         userDefaultProfileImage              <- readProp "default_profile_image" rawObj
         userDescription                      <- runNullOrUndefined <$> readProp "description" rawObj
         userFavoritesCount                   <- readProp "favourites_count" rawObj
         userFollowRequestSent                <- runNullOrUndefined <$> readProp "follow_request_sent" rawObj
         userFollowing                        <- runNullOrUndefined <$> readProp "following" rawObj
         userFollowersCount                   <- readProp "followers_count" rawObj
         userFriendsCount                     <- readProp "friends_count" rawObj
         userGeoEnabled                       <- readProp "geo_enabled" rawObj
         userId                               <- readProp "id" rawObj
         userIsTranslator                     <- readProp "is_translator" rawObj
         userLang                             <- readProp "lang" rawObj
         userListedCount                      <- readProp "listed_count" rawObj
         userLocation                         <- runNullOrUndefined <$> readProp "location" rawObj
         userName                             <- readProp "name" rawObj
         userNotifications                    <- runNullOrUndefined <$> readProp "notifications" rawObj
         userProfileBackgroundColor           <- runNullOrUndefined <$> readProp "profile_background_color" rawObj
         userProfileBackgroundImageURL        <- runNullOrUndefined <$> readProp "profile_background_image_url" rawObj
         userProfileBackgroundImageURLHttps   <- runNullOrUndefined <$> readProp "profile_background_image_url_https" rawObj
         userProfileBackgroundTile            <- runNullOrUndefined <$> readProp "profile_background_tile" rawObj
         userProfileBannerURL                 <- runNullOrUndefined <$> readProp "profile_banner_url" rawObj
         userProfileImageURL                  <- runNullOrUndefined <$> readProp "profile_image_url" rawObj
         userProfileImageURLHttps             <- runNullOrUndefined <$> readProp "profile_image_url_https" rawObj
         userProfileLinkColor                 <- readProp "profile_link_color" rawObj
         userProfileSidebarBorderColor        <- readProp "profile_sidebar_border_color" rawObj
         userProfileSidebarFillColor          <- readProp "profile_sidebar_fill_color" rawObj
         userProfileTextColor                 <- readProp "profile_text_color" rawObj
         userProfileUseBackgroundImage        <- readProp "profile_use_background_image" rawObj
         userProtected                        <- readProp "protected" rawObj
         userScreenName                       <- readProp "screen_name" rawObj
         userShowAllInlineMedia               <- runNullOrUndefined <$> readProp "show_all_inline_media" rawObj
         userStatusesCount                    <- readProp "statuses_count" rawObj
         userTimeZone                         <- runNullOrUndefined <$> readProp "time_zone" rawObj
         userURL                              <- runNullOrUndefined <$> readProp "url" rawObj
         userUtcOffset                        <- runNullOrUndefined <$> readProp "utc_offset" rawObj
         userVerified                         <- readProp "verified" rawObj
         userWithheldInCountries              <- runNullOrUndefined <$> readProp "withheld_in_countries" rawObj
         userWithheldScope                    <- runNullOrUndefined <$> readProp "withheld_scope" rawObj

         return $ User  { userContributorsEnabled              : userContributorsEnabled
                        , userCreatedAt                        : userCreatedAt
                        , userDefaultProfile                   : userDefaultProfile
                        , userDefaultProfileImage              : userDefaultProfileImage
                        , userDescription                      : userDescription
                        , userFavoritesCount                   : userFavoritesCount
                        , userFollowRequestSent                : userFollowRequestSent
                        , userFollowing                        : userFollowing
                        , userFollowersCount                   : userFollowersCount
                        , userFriendsCount                     : userFriendsCount
                        , userGeoEnabled                       : userGeoEnabled
                        , userId                               : userId
                        , userIsTranslator                     : userIsTranslator
                        , userLang                             : userLang
                        , userListedCount                      : userListedCount
                        , userLocation                         : userLocation
                        , userName                             : userName
                        , userNotifications                    : userNotifications
                        , userProfileBackgroundColor           : userProfileBackgroundColor
                        , userProfileBackgroundImageURL        : userProfileBackgroundImageURL
                        , userProfileBackgroundImageURLHttps   : userProfileBackgroundImageURLHttps
                        , userProfileBackgroundTile            : userProfileBackgroundTile
                        , userProfileBannerURL                 : userProfileBannerURL
                        , userProfileImageURL                  : userProfileImageURL
                        , userProfileImageURLHttps             : userProfileImageURLHttps
                        , userProfileLinkColor                 : userProfileLinkColor
                        , userProfileSidebarBorderColor        : userProfileSidebarBorderColor
                        , userProfileSidebarFillColor          : userProfileSidebarFillColor
                        , userProfileTextColor                 : userProfileTextColor
                        , userProfileUseBackgroundImage        : userProfileUseBackgroundImage
                        , userProtected                        : userProtected
                        , userScreenName                       : userScreenName
                        , userShowAllInlineMedia               : userShowAllInlineMedia
                        , userStatusesCount                    : userStatusesCount
                        , userTimeZone                         : userTimeZone
                        , userURL                              : userURL
                        , userUtcOffset                        : userUtcOffset
                        , userVerified                         : userVerified
                        , userWithheldInCountries              : userWithheldInCountries
                        , userWithheldScope                    : userWithheldScope }

instance isForeignTweetElement :: IsForeign TweetElement where
    read data_ = do
        k <- readProp "type" data_
        v <- readProp "value" data_

        return $ toTweetToken k v

instance isForeignAuthor :: IsForeign Author where
    read data_ = do
        n <- readProp "name" data_
        authorId <- readProp "authorId" data_
        screen_name <- readProp "screen_name" data_
        default_img <- readProp "default_profile_image" data_
        img <- readProp "profile_image_url" data_

        return $ Author { name: n
                        , authorId: authorId
                        , screen_name: screen_name
                        , default_profile_image: default_img
                        , profile_image_url: img
                        }

instance isForeignTweet :: IsForeign Tweet where
    read x = do
        t <- readProp "text" x
        c <- readProp "created_at" x
        i <- readProp "id" x
        s <- readProp "id_str" x
        a <- readProp "user" x
        e <- readProp "entities" x
        r <- runNullOrUndefined <$> "retweet" `readProp` x
        f <- readProp "favorited" x
        w <- readProp "retweeted" x

        return $ Tweet { text: t
                       , created_at: c
                       , id: i
                       , id_str: s
                       , user: a
                       , entities: e
                       , retweet: r
                       , favorited: f
                       , retweeted: w
                       }

instance isForeignTASettings :: IsForeign TASettings where
    read x = do
        t <- readProp "screen_name" x
        return $ TASettings { accScreenName: t }

--------------------------------------------------------------------------------

readFeedMessage x | readProp "tag" x == Right "TweetMessage" = do
    y <- readProp "contents" x
    return $ TweetMessage y

readFeedMessage x | readProp "tag" x == Right "UserMessage" = do
    y <- readProp "contents" x
    return $ UserMessage y

readFeedMessage x | readProp "tag" x == Right "SettingsMessage" = do
    y <- readProp "contents" x
    return $ SettingsMessage y

readFeedMessage x | readProp "tag" x == Right "FriendsListMessage" = do
    y <- readProp "contents" x
    return $ FriendsListMessage y

readFeedMessage x | readProp "tag" x == Right "ErrorMessage" = do
    y <- readProp "contents" x
    return $ ErrorMessage y

instance isForeignFeedMessage :: IsForeign FeedMessage where
    read = readFeedMessage

--------------------------------------------------------------------------------

readX data_ | hasOwnProperty "errTitle" data_ = do
    m <- readProp "errMessage" data_
    t <- readProp "errTitle" data_
    return $ ResponseError {errTitle: t, errMessage: m}

readX data_ | hasOwnProperty "okTitle" data_ = do
    ts <- readProp "okFeedMessages" data_
    t  <- readProp "okTitle" data_
    return $ ResponseSuccess {okTitle: t, okFeedMessages: ts}

readX data_ | hasOwnProperty "toTitle" data_ = do
    m <- readProp "toMessage" data_
    t <- readProp "toTitle" data_
    return $ ResponseTimeout {toTitle: t, toMessage: m}

readX data_ | hasOwnProperty "uiTitle" data_ = do
    d <- readProp "uiData" data_
    t <- readProp "uiTitle" data_
    return $ ResponseUserinfo {uiTitle: t, uiData: d}

readX data_ | otherwise = return $ ResponseError { errTitle: "Other error"
                                                 , errMessage: "Can't parse ApiResponse response" }

instance isForeignResponseError :: IsForeign ApiResponse where
  read = readX


instance isForeignCheckResponse :: IsForeign CheckResponse where
    read x = do
        c <- "unreadCount" `readProp` x

        return $ CheckResponse { unreadTitle: (show c) ++ " new tweets", unreadCount: c }

instance isForeignEntities :: IsForeign Entities where
  read x = do
    u <- "urls" `readProp` x
    h <- "hashtags" `readProp` x
    m <- runNullOrUndefined <$> "media" `readProp` x

    return $ Entities { urls: u, hashtags: h, media: m }

instance isForeignEntityUrl :: IsForeign EntityUrl where
  read x = do
    eExpandedUrl <- "expanded_url" `readProp` x
    eUrl         <- "url" `readProp` x
    eIndices     <- "indices" `readProp` x
    --eDisplayUrl <- "eDisplayUrl" `readProp` x

    return $ EntityUrl { eExpandedUrl: eExpandedUrl
                       , eUrl: eUrl
                       , eIndices: eIndices
                       }

instance isForeignHashtag :: IsForeign EntityHashtag where
  read x = do
    t <- "text" `readProp` x
    i <- "indices" `readProp` x

    return $ EntityHashtag { hText: t, hIndices: i }

instance isForeignMedia :: IsForeign EntityMedia where
  read x = do
    type_         <- "type" `readProp` x
    indices       <- "indices" `readProp` x
    url           <- "url" `readProp` x
    media_url     <- "media_url" `readProp` x
    display_url   <- "display_url" `readProp` x
    expanded_url  <- "expanded_url" `readProp` x
    sizes         <- "sizes" `readProp` x

    return $ EntityMedia { mType: type_
                         , mIndices: indices
                         , mUrl: url
                         , mMediaUrl: media_url
                         , mDisplayUrl: display_url
                         , mExpandedUrl: expanded_url
                         , mSizes: sizes
                         }

instance isForeignMediaSize :: IsForeign EntityMediaSize where
  read x = do
    h <- "h" `readProp` x
    w <- "w" `readProp` x
    resize <- "resize" `readProp` x

    return $ EntityMediaSize { h: h, w: w, resize: resize}

instance isForeignMediaSizes :: IsForeign EntityMediaSizes where
  read x = do
    thumb   <- "thumb" `readProp` x
    large   <- "large" `readProp` x
    medium  <- "medium" `readProp` x
    small   <- "small" `readProp` x

    return $ EntityMediaSizes {thumb: thumb, large: large, medium: medium, small: small}

justTweets :: Array FeedMessage -> Array Tweet
justTweets xs = (\(TweetMessage t) -> t) <$> filter justTweetMessage xs

justTweetMessage :: FeedMessage -> Boolean
justTweetMessage (TweetMessage _) = true
justTweetMessage _                = false

justUsers :: Array FeedMessage -> Array User
justUsers xs = (\(UserMessage t) -> t) <$> filter justUserMessage xs

justUserMessage :: FeedMessage -> Boolean
justUserMessage (UserMessage _) = true
justUserMessage _               = false

justSettings :: Array FeedMessage -> Array TASettings
justSettings xs = (\(SettingsMessage t) -> t) <$> filter justSettingsMessage xs

justSettingsMessage :: FeedMessage -> Boolean
justSettingsMessage (SettingsMessage _) = true
justSettingsMessage _                   = false

justFriendsLists :: Array FeedMessage -> Array User
justFriendsLists xs = concat $ (\(FriendsListMessage t) -> t) <$> filter justFriendsListMessage xs

justFriendsListMessage :: FeedMessage -> Boolean
justFriendsListMessage (FriendsListMessage _) = true
justFriendsListMessage _                      = false

justErrors :: Array FeedMessage -> Array ApiResponse
justErrors xs = (\(ErrorMessage t) -> t) <$> filter justErrorMessage xs

justErrorMessage :: FeedMessage -> Boolean
justErrorMessage (ErrorMessage _) = true
justErrorMessage _                = false

fromResponse :: String -> ApiResponse
fromResponse x = case (readJSON x :: F ApiResponse) of
  Left err -> ResponseError {errTitle: "Other error", errMessage: ("Can't parse response: " ++ toString err)}
  Right resp -> resp

fromWsMessage :: String -> Array FeedMessage
fromWsMessage s = case (readJSON s :: F (Array FeedMessage)) of
    Left err -> []
    Right ts -> ts

--------------------------------------------------------------------------------
successM m = Success m (runUUID $ getUUID)
errorM m = Error m (runUUID $ getUUID)

setMessage :: forall eff. Ref State -> StatusMessage -> Eff (ref :: REF | eff) Unit
setMessage state msg = do
    s <- readState state
    writeState state (s # messagesL .~ ((s ^. messagesL) ++ [msg]))

initialState :: State
initialState = State { feed: AFeed { oldFeed: OldFeed []
                                   , currentFeed: CurrentFeed []
                                   , newFeed: NewFeed [] }
                     , extraFeed: Nothing
                     , historyButtonDisabled: false
                     , userInfo: UserInfo { visible: false
                                          , followRequestActive: false
                                          , userdata: Nothing }
                     , myInfo: MyInfo { visible: false
                                      , friends: []
                                      , userInfo: Nothing }
                     , writeInput: WriteInput { visible: false
                                              , disabled: false
                                              , value: "~"
                                              , replyTo: Nothing }
                     , searchInput: SearchInput { visible: false
                                                , value: "" }
                     , contextMenu: ContextMenu { visible: false
                                                , x: 0
                                                , y: 0
                                                , tweetId: Nothing }
                     , account: Account { settings: Nothing
                                        , friends: Nothing }
                     , errors: [] }

favoritedL :: LensP Tweet Boolean
favoritedL = lens (\(Tweet x) -> x.favorited)
                  (\(Tweet x) v -> Tweet (x { favorited = v }))

retweetedL :: LensP Tweet Boolean
retweetedL = lens (\(Tweet x) -> x.retweeted)
                  (\(Tweet x) v -> Tweet (x { retweeted = v }))

accountSettingsL :: LensP Account (Maybe TASettings)
accountSettingsL = lens (\(Account x) -> x.settings)
                        (\(Account x) v -> Account (x { settings = v }))

accountFriendsL :: LensP Account (Maybe (Set User))
accountFriendsL = lens (\(Account x) -> x.friends)
                       (\(Account x) v -> Account (x { friends = v }))

accountL :: LensP State Account
accountL = lens (\(State x) -> x.account)
                (\(State x) v -> State (x { account = v }))

searchInputVisibleL :: LensP SearchInput Boolean
searchInputVisibleL = lens (\(SearchInput x) -> x.visible)
                           (\(SearchInput x) v -> SearchInput (x { visible = v }))

searchInputValueL :: LensP SearchInput String
searchInputValueL = lens (\(SearchInput x) -> x.value)
                         (\(SearchInput x) v -> SearchInput (x { value = v }))

searchInputL :: LensP State SearchInput
searchInputL = lens (\(State s) -> s.searchInput)
                   (\(State s) wi -> State (s { searchInput = wi }))

myInfoL :: LensP State MyInfo
myInfoL = lens (\(State s) -> s.myInfo)
               (\(State s) my -> State (s { myInfo = my }))

myInfoVisibleL :: LensP MyInfo Boolean
myInfoVisibleL = lens (\(MyInfo x) -> x.visible)
                      (\(MyInfo x) v -> MyInfo (x { visible = v }))

userInfoL :: LensP State UserInfo
userInfoL = lens (\(State s) -> s.userInfo)
                 (\(State s) ui -> State (s { userInfo = ui }))

userInfoVisibleL :: LensP UserInfo Boolean
userInfoVisibleL = lens (\(UserInfo x) -> x.visible)
                        (\(UserInfo x) v -> UserInfo (x { visible = v }))

userInfoUserdataL :: LensP UserInfo (Maybe User)
userInfoUserdataL = lens (\(UserInfo x) -> x.userdata)
                         (\(UserInfo x) d -> UserInfo (x { userdata = d }))

writeInputDisabledL :: LensP WriteInput Boolean
writeInputDisabledL = lens (\(WriteInput x) -> x.disabled)
                           (\(WriteInput x) v -> WriteInput (x { disabled = v }))

writeInputVisibleL :: LensP WriteInput Boolean
writeInputVisibleL = lens (\(WriteInput x) -> x.visible)
                          (\(WriteInput x) v -> WriteInput (x { visible = v }))

writeInputValueL :: LensP WriteInput String
writeInputValueL = lens (\(WriteInput x) -> x.value)
                        (\(WriteInput x) v -> WriteInput (x { value = v }))

contextMenuL :: LensP State ContextMenu
contextMenuL = lens (\(State s) -> s.contextMenu)
                    (\(State s) cm -> State (s { contextMenu = cm }))

writeInputL :: LensP State WriteInput
writeInputL = lens (\(State s) -> s.writeInput)
                   (\(State s) wi -> State (s { writeInput = wi }))

messagesL :: LensP State (Array StatusMessage)
messagesL = lens (\(State s) -> s.errors)
                 (\(State s) msgs -> State (s {errors = msgs}))

hbdL :: LensP State Boolean
hbdL = lens (\(State s) -> s.historyButtonDisabled)
            (\(State s) val -> State (s { historyButtonDisabled = val }))

feedL :: LensP State AFeed
feedL = lens (\(State s) -> s.feed)
             (\(State s) f -> State (s {feed = f}))

extraFeedL :: LensP State (Maybe BFeed)
extraFeedL = lens (\(State s) -> s.extraFeed)
                  (\(State s) ef -> State (s {extraFeed = ef}))

newFeedL :: LensP AFeed (NewFeed Tweet)
newFeedL = lens (\(AFeed s) -> s.newFeed)
                (\(AFeed s) ts -> AFeed (s { newFeed = ts }))

oldFeedL :: LensP AFeed (OldFeed Tweet)
oldFeedL = lens (\(AFeed s) -> s.oldFeed)
                (\(AFeed s) ts -> AFeed (s { oldFeed = ts }))

currentFeedL :: LensP AFeed (CurrentFeed Tweet)
currentFeedL = lens (\(AFeed s) -> s.currentFeed)
                    (\(AFeed s) ts -> AFeed (s { currentFeed = ts }))

resetContextMenu state = do
     s <- readState state
     writeState state (s # contextMenuL .~ ContextMenu { visible: false
                                                       , x: 0
                                                       , y: 0
                                                       , tweetId: Nothing })

-- TODO get rid of singleton global state
stateObservable = getNewObservable initialState

readState = readRef

writeState state value = do
    writeRef state value
    let x = publishToObservable stateObservable value
    pure unit

enableHistoryButton = toggleHistoryButton false
disableHistoryButton = toggleHistoryButton true

toggleHistoryButton x state = do
    s <- readState state
    writeState state (s # hbdL .~ x)
