{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}

module BL.Core (
    Url
  , Tweet(..)
  , Author(..)
  , Username
  , ApiError(..)
  , BL.Types.Entities(..)
  , EntityUrl(..)
  , EntityMedia(..)
  , TweetElement(..)
  , JsonApiError(..)
  , JsonResponse(..)
  , TweetId
  , retweetStatusToTweet
  , handleIncomingFeedMessages
  , statusToTweet
  , saveLastSeen
  , saveLastSeenAsync
  , updateFeed
  , updateFeedSync
  , readUserstream
  , readUserInfo
  , retweetUrl
  , followUser
  , unfollowUser
  , getRunTime
  , tweetUrl
  , fetchContext
  , replyUrl
  , readHistory
  , getStatus
  , writeApi
  , sendFetchAccountRequest
  , readApi
  , starUrl
  , twInfo
  ) where

import           Data.Text                      (pack, unpack)

import           System.IO
import           Web.Authenticate.OAuth

import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import           Network.HTTP.Conduit

import           Control.Exception.Lifted       (try)

import           Control.Applicative
import           Control.Concurrent             (MVar, ThreadId, forkIO,
                                                 killThread, modifyMVar_,
                                                 newEmptyMVar, newMVar, putMVar,
                                                 readMVar, swapMVar, takeMVar,
                                                 threadDelay, tryTakeMVar)
import           Control.Monad
import           Control.Monad.Trans            (liftIO)
import           Network.HTTP.Types             (Status (..))
import qualified Network.HTTP.Types             as HTTP

import qualified BL.CloudDataLayer              as CDL
import qualified BL.DataLayer                   as DL
import           BL.Parser                      (parseTweet)
import           BL.Types
import qualified Config                         as CFG
import           Data.Aeson
import qualified Data.ByteString.Char8          as BS
import           Data.Conduit
import qualified Data.Conduit.List              as CL
import           Data.HashMap.Strict
import           Data.Int                       (Int64)
import           Data.Text.Encoding             (decodeUtf8)
import           Data.Time.Clock                (NominalDiffTime, UTCTime (..),
                                                 diffUTCTime, getCurrentTime,
                                                 secondsToDiffTime)
import           GHC.Generics
import           Prelude                        hiding (error)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Web.Twitter.Conduit
import           Web.Twitter.Conduit.Api        (usersShow)
import           Web.Twitter.Conduit.Base       (call)
import           Web.Twitter.Conduit.Parameters (UserParam (ScreenNameParam))
import           Web.Twitter.Types
import qualified Web.Twitter.Types              as TT

logRealm = "Core"

info  = infoM    logRealm
warn  = warningM logRealm
debug = debugM   logRealm
error = errorM   logRealm

oauthToken :: Cfg -> OAuth
oauthToken cfg = twitterOAuth { oauthConsumerKey = BS.pack (cfgOauthConsumerKey cfg)
                              , oauthConsumerSecret = BS.pack (cfgOauthConsumerSecret cfg) }

oauthCredential :: Cfg -> Credential
oauthCredential cfg = newCredential (B8.pack (cfgAccessToken cfg)) (B8.pack (cfgAccessTokenSecret cfg))

twInfo :: Cfg -> TWInfo
twInfo cfg = setCredential (oauthToken cfg) (oauthCredential cfg) def

retweetStatusToTweet :: TT.RetweetedStatus -> Tweet
retweetStatusToTweet s = Tweet { text               = parseTweet $ TT.rsText s
                               , created_at         = pack $ show $ TT.rsCreatedAt s
                               , id_                = fromIntegral (TT.rsId s) :: Int64
                               , id_str             = show $ TT.rsId s
                               , user               = statusUserToAuthor $ TT.rsUser s
                               , entities           = statusEntitiesToEntities $ TT.rsEntities s
                               , retweet            = Just $ statusToTweet $ TT.rsRetweetedStatus s
                               , status_favorited   = Nothing
                               , status_retweeted   = Nothing }

statusRetweetToRetweet :: Maybe TT.Status -> Maybe Tweet
statusToTweet :: TT.Status -> Tweet
statusToTweet s = Tweet { text              = parseTweet $ TT.statusText s
                        , created_at        = pack $ show $ TT.statusCreatedAt s
                        , id_               = fromIntegral (TT.statusId s) :: Int64
                        , id_str            = show $ TT.statusId s
                        , user              = statusUserToAuthor $ TT.statusUser s
                        , entities          = statusEntitiesToEntities $ TT.statusEntities s
                        , retweet           = statusRetweetToRetweet $ TT.statusRetweetedStatus s
                        , status_favorited  = TT.statusFavorited s
                        , status_retweeted  = TT.statusRetweeted s }

statusUserToAuthor :: TT.User -> Author
statusUserToAuthor s = Author (TT.userName s)
                              (TT.userId s)
                              (TT.userScreenName s)
                              (TT.userDefaultProfileImage s)
                              (avatarUrl $ TT.userProfileImageURL s)
  where
  avatarUrl x = case x of
      Nothing -> "http://a0.twimg.com/sticky/default_profile_images/default_profile_6_normal.png"
      Just y -> unpack y


statusEntitiesToEntities :: Maybe TT.Entities -> BL.Types.Entities
statusEntitiesToEntities Nothing  = BL.Types.Entities [] [] Nothing
statusEntitiesToEntities (Just s) = BL.Types.Entities (xUrl <$> TT.enURLs s)
                                                      (xHashtag <$> TT.enHashTags s)
                                                      (Just $ xMedia <$> TT.enMedia s)
  where
  xUrl (TT.Entity x _) = EntityUrl (unpack $ TT.ueExpanded x) (unpack $ TT.ueURL x) [] (unpack $ TT.ueDisplay x)
  xHashtag (TT.Entity x _) = EntityHashtag (TT.hashTagText x) []
  xMedia (TT.Entity x _) = EntityMedia (unpack $ TT.meType x)
                                       []
                                       (unpack $ TT.ueExpanded $ TT.meURL x)
                                       (unpack $ TT.meMediaURL x)
                                       (unpack $ TT.meMediaURL x)
                                       (unpack $ TT.meMediaURL x)
                                       (xSizes $ TT.meSizes x)
  xSizes x = EntityMediaSizes (xSize $ x ! "thumb" )
                              (xSize $ x ! "large" )
                              (xSize $ x ! "medium" )
                              (xSize $ x ! "small" )
  xSize x = EntityMediaSize (TT.msHeight x) (TT.msWidth x) (unpack $ TT.msResize x)

statusRetweetToRetweet s = case s of
    Nothing -> Nothing
    Just s -> Just $ statusToTweet s


--   see <https://dev.twitter.com/docs/platform-objects/tweets>.
instance FromJSON Tweet where
  parseJSON (Object x) = do
    text        <- x .: "text"
    created_at  <- x .: "created_at"
    id_         <- x .: "id"
    id_str      <- x .: "id_str"
    user        <- x .: "user"
    entities    <- x .: "entities"
    retweet     <- x .:? "retweeted_status"
    favorited   <- x .:? "favorited"
    retweeted   <- x .:? "retweeted"

    return $ Tweet (parseTweet text) created_at id_ id_str user entities retweet favorited retweeted

  parseJSON _ = fail "tweet is expected to be an object"

instance ToJSON Tweet where
  toJSON x = object [ "text"       .= text x
                    , "created_at" .= created_at x
                    , "id"         .= BL.Types.id_ x
                    , "id_str"     .= id_str x
                    , "user"       .= user x
                    , "entities"   .= entities x
                    , "retweet"    .= retweet x
                    , "favorited"  .= status_favorited x
                    , "retweeted"  .= status_retweeted x
                    ]

instance FromJSON TASettings where
  parseJSON (Object x) = do
    sn <- x .: "screen_name"
    return TASettings { accScreenName = sn }

  parseJSON _ = fail "Account Settings is expected to be an object"

instance ToJSON TASettings where
  toJSON x = object [ "screen_name" .= accScreenName x ]

instance FromJSON JsonApiError
instance ToJSON JsonApiError

instance FromJSON FeedMessage
instance ToJSON FeedMessage

instance FromJSON JsonResponse
instance ToJSON JsonResponse

instance FromJSON JsonUserInfo
instance ToJSON JsonUserInfo

-- instance FromJSON User
-- instance ToJSON User

instance FromJSON BL.Types.Entities where
  parseJSON (Object x) = BL.Types.Entities <$> x .: "urls"
                                           <*> x .: "hashtags"
                                           <*> x .:? "media"
  parseJSON _ = fail "entities is expected to be an object"

instance ToJSON BL.Types.Entities where
  toJSON x = object [ "urls"     .= urls x
                    , "hashtags" .= hashtags x
                    , "media"    .= media x
                    ]

instance FromJSON EntityUrl where
  parseJSON (Object x) = EntityUrl <$> x .: "expanded_url"
                                   <*> x .: "url"
                                   <*> x .: "indices"
                                   <*> x .: "display_url"
  parseJSON _ = fail "entity url is expected to be an object"

instance ToJSON EntityUrl where
    toJSON x = object [ "expanded_url" .= eExpandedUrl x
                      , "url"          .= eUrl x
                      , "indices"      .= eIndices x ]

instance FromJSON EntityHashtag where
  parseJSON (Object x) = EntityHashtag <$> x .: "text" <*> x .: "indices"
  parseJSON _ = fail "entity hashtag is expected to be an object"

instance ToJSON EntityHashtag where
  toJSON x = object [ "text"    .= hText x
                    , "indices" .= hIndices x ]

instance FromJSON EntityMedia where
  parseJSON (Object x) = EntityMedia <$> x .: "type"
                                     <*> x .: "indices"
                                     <*> x .: "url"
                                     <*> x .: "media_url"
                                     <*> x .: "display_url"
                                     <*> x .: "expanded_url"
                                     <*> x .: "sizes"
  parseJSON _ = fail "entity media is expected to be an object"

instance ToJSON EntityMedia where
  toJSON x = object [ "type"         .= mType x
                    , "indices"      .= mIndices x
                    , "url"          .= mUrl x
                    , "media_url"    .= mMediaUrl x
                    , "display_url"  .= mDisplayUrl x
                    , "expanded_url" .= mExpandedUrl x
                    , "sizes"        .= mSizes x
                    ]

instance FromJSON EntityMediaSize
instance ToJSON EntityMediaSize

instance FromJSON EntityMediaSizes
instance ToJSON EntityMediaSizes

instance FromJSON Author where
  parseJSON (Object x) = Author <$> x .: "name"
                                <*> x .: "id"
                                <*> x .: "screen_name"
                                <*> x .: "default_profile_image"
                                <*> x .: "profile_image_url"
  parseJSON _ = fail "Author is expected to be an object"

instance ToJSON Author where
  toJSON x = object [ "name"                    .= name x
                    , "authorId"                .= authorId x
                    , "screen_name"             .= screen_name x
                    , "default_profile_image"   .= default_profile_image x
                    , "profile_image_url"       .= profile_image_url x
                    ]


instance FromJSON JsonUnreadCount
instance ToJSON JsonUnreadCount

toTweetToken :: String -> String -> TweetElement
toTweetToken "AtUsername"   = AtUsername
toTweetToken "Link"         = Link
toTweetToken "PlainText"    = PlainText
toTweetToken "Hashtag"      = Hashtag
toTweetToken "Spaces"       = Spaces
toTweetToken "Unparsable"   = Unparsable

instance FromJSON TweetElement where
    parseJSON (Object x) = toTweetToken <$> x .: "type" <*> x .: "value"
    parseJSON _ = mzero

instance ToJSON TweetElement where
    toJSON (AtUsername s) = object [ "type" .= ("AtUsername" :: String), "value" .= s ]
    toJSON (Link u)       = object [ "type" .= ("Link" :: String),       "value" .= u ]
    toJSON (PlainText s)  = object [ "type" .= ("PlainText" :: String),  "value" .= s ]
    toJSON (Hashtag s)    = object [ "type" .= ("Hashtag" :: String),    "value" .= s ]
    toJSON (Retweet)      = object [ "type" .= ("Retweet" :: String),    "value" .= ("RT" :: String) ]
    toJSON (Spaces s)     = object [ "type" .= ("Spaces" :: String),     "value" .= s ]
    toJSON (Unparsable s) = object [ "type" .= ("Unparsable" :: String), "value" .= s ]

--------------------------------------------------------------------------------

accountSettingsUrl :: Url
accountSettingsUrl = "https://api.twitter.com/1.1/account/settings.json"

retweetUrl :: TweetId -> Url
retweetUrl x = "https://api.twitter.com/1.1/statuses/retweet/" ++ show x ++ ".json"

starUrl :: TweetId -> Url
starUrl x = "https://api.twitter.com/1.1/favorites/create.json?id=" ++ show x

tweetUrl :: TweetBody -> Url
tweetUrl status = "https://api.twitter.com/1.1/statuses/update.json?status=" ++ B8.unpack status

replyUrl :: TweetBody -> B8.ByteString -> Url
replyUrl status reply_to_id = "https://api.twitter.com/1.1/statuses/update.json?status=" ++ B8.unpack status
                                    ++ "&in_reply_to_status_id=" ++ B8.unpack reply_to_id

userTimeline :: Username -> Int -> Feed
userTimeline name 0 = UserTimeline $
  "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
userTimeline name count = UserTimeline $
  "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  ++ name ++ "&count=" ++ show count

homeTimeline :: Int -> Feed
homeTimeline 0 = HomeTimeline "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimeline count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count

homeTimelineSince :: TweetId -> Feed
homeTimelineSince 0 = HomeTimeline "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimelineSince tid = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=200&since_id=" ++ show tid

homeTimelineSinceCount :: TweetId -> Int -> Feed
homeTimelineSinceCount 0 0 = HomeTimeline "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimelineSinceCount tid 0 = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?since_id=" ++ show tid
homeTimelineSinceCount 0 count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count
homeTimelineSinceCount tid count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?since_id="
  ++ show tid ++ "&count=" ++ show count

homeTimelineMaxidCount :: TweetId -> Int -> Feed
homeTimelineMaxidCount 0 0 = HomeTimeline
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=20"
homeTimelineMaxidCount tid 0 = HomeTimeline $
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=20&max_id=" ++ show tid
homeTimelineMaxidCount 0 count = HomeTimeline $
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count
homeTimelineMaxidCount tid count = HomeTimeline $
    "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count ++ "&max_id=" ++ show tid

getUpdateFeedUrl :: DL.MyDb -> String -> IO Feed
getUpdateFeedUrl db url = do
  xs <- CDL.readCloudDb url
  info $ "Read from cloud db with result of length: " ++ show (length <$> xs)

  let z = case xs of
          Left x   -> 0
          Right [] -> 0
          Right y  -> CDL.lastSeenId (maximum y)

  debug $ "Selected max tweet id: " ++ show z
  return $ homeTimelineSince z

--------------------------------------------------------------------------------

justTweets :: FeedState -> [Tweet]
justTweets xs = (\(TweetMessage t) -> t) <$> Prelude.filter justTweetMessage xs

justTweetMessage :: FeedMessage -> Bool
justTweetMessage (TweetMessage _) = True
justTweetMessage _                = False

saveLastSeenAsync :: DL.MyDb -> FeedState -> Cfg -> IO ThreadId
saveLastSeenAsync db ts cfg = forkIO $ saveLastSeen db ts cfg >> return ()

saveLastSeen :: DL.MyDb -> FeedState -> Cfg -> IO (Either CDL.CloudDataLayerError CDL.WriteResponse)
saveLastSeen _ ts _ | justTweets ts == [] = return $ Right $ CDL.WriteSuccess "write skipped"
saveLastSeen db ts cfg = do
    now <- getCurrentTime
    res <- CDL.writeCloudDb (CDL.CloudDbStoreItem (getMaxId $ justTweets ts) (show now)) cfg -- now
    info $ "Wrote to cloud db with result: " ++ show res
    return res

    where
    getMaxId :: [Tweet] -> TweetId
    --getMaxId [] = ?
    getMaxId ts = maximum (BL.Types.id_ <$> ts)

putToClientQueue q ms = do
    maybeOldMs <- tryTakeMVar q
    case maybeOldMs of
        Just oldms -> putMVar q $ oldms ++ ms
        Nothing    -> putMVar q ms

handleIncomingFeedMessages :: DL.MyDb -> MVar FeedState -> [FeedMessage] -> IO ()
handleIncomingFeedMessages _ _ [] = do
    info "Got no updated feed messagess, skipping saving"
    return ()
handleIncomingFeedMessages _ fv ts = do
    info $ "Got " ++ show (length ts) ++ " updated feed messages/s"
    putToClientQueue fv ts

readUserstream :: ScreenName -> Int -> Cfg -> IO (Either (ApiError HttpException) FeedState)
readUserstream sn count cfg = do
    info $ "reading userstream where ScreenName=" ++ show sn ++ " and count=" ++ show count
    (_, res) <- readApi (BL.Core.userTimeline (unpack sn) count) cfg
    return ((TweetMessage <$>) <$> res)

--readuserInfo :: ScreenName -> IO (Either (ApiError HttpException) UserInfo)
readUserInfo sn cfg = withManager $ \mgr -> do
    res <- callWithResponse (twInfo cfg) mgr $ usersShow (ScreenNameParam sn)
    case Web.Twitter.Conduit.responseStatus res of
        HTTP.Status {statusCode = 200, statusMessage = _ } ->
            return $ Right $ Web.Twitter.Conduit.responseBody res -- res :: User

        HTTP.Status {statusCode=code, statusMessage=msg} ->
            return $ Left $ ApiError $ "Twitter API returned bad status code: " ++ show code
                                                                         ++ " " ++ show msg

followUser sn   = followUnfollowUser sn (friendshipsCreate  (ScreenNameParam sn))
unfollowUser sn = followUnfollowUser sn (friendshipsDestroy (ScreenNameParam sn))

followUnfollowUser sn req cfg = withManager $ \mgr -> do
    res0 <- callWithResponse (twInfo cfg) mgr req
    liftIO $ print $ Web.Twitter.Conduit.responseStatus res0

    case Web.Twitter.Conduit.responseStatus res0 of
        HTTP.Status {statusCode = 200, statusMessage = _ } -> do
            -- can't get rid of this 'cause on unfollow wtitter doesn't send unfollowed user info
            -- TODO handle errors
            res <- call (twInfo cfg) mgr $ usersShow (ScreenNameParam sn)
            return $ Right res -- res :: MonadResource User

        HTTP.Status {statusCode=code, statusMessage=msg} ->
            return $ Left $ ApiError $ "Twitter API returned bad status code: " ++ show code
                                                                         ++ " " ++ show msg



readHistory :: TweetId -> Int -> Cfg -> IO (Either (ApiError HttpException) FeedState)
readHistory maxid count cfg = do
    info $ "reading history where maxid=" ++ show maxid ++ " and count=" ++ show count
    (_, res) <- readApi (homeTimelineMaxidCount maxid count) cfg
    return $ (TweetMessage <$>) <$> res

sendFetchAccountRequest :: MVar UTCTime -> IO ()
sendFetchAccountRequest accv = do
    now <- getCurrentTime
    debug $ "***Putting an account fetch request at " ++ show now
    _ <- forkIO $ putMVar accv now -- avoid blocking caller
    return ()

updateFeed :: MVar UTCTime -> IO ()
updateFeed uv = do
    now <- getCurrentTime
    debug $ "***Putting an update request at " ++ show now
    _ <- forkIO $ putMVar uv now -- avoid blocking caller
    return ()

updateFeedSync :: DL.MyDb -> MVar FeedState -> Cfg -> IO ()
updateFeedSync db fv cfg = do
    info "Updating feed"
    feedUrl <- getUpdateFeedUrl db (cfgCloudDbUrl cfg)
    doreq feedUrl db fv (0 :: Int)

  where
  doreq f db fv iter = do
    (_, res) <- readApi f cfg
    case res of
      Right ts -> handleIncomingFeedMessages db fv (reverse $ TweetMessage <$> ts)

      Left (TransportError (FailedConnectionException2 _ _ _ ex)) -> if iter < CFG.updateRetryCount
        then do
          error $ "Http error at update attempt " ++ show iter ++ "/"++ show CFG.updateRetryCount ++ ": " ++ show ex
                ++ ". Retrying in " ++ show CFG.updateRetryDelay ++ "ms"
          threadDelay CFG.updateRetryDelay
          doreq f db fv (iter + 1)

        else
          error $ "error: update retry count exceeded: error was: " ++ show ex

      Left (TransportError ex) -> error $ "error: transport error: " ++ show ex
      Left (ApiError msg)      -> error $ "error: api error: " ++ show msg

fetchContext :: MVar FeedState -> Cfg -> IO ()
fetchContext fv cfg = do
    res <- fetchSettings accountSettingsUrl cfg
    case res of
        Left err -> putToClientQueue fv [ErrorMessage err]
        Right settings -> do
            putToClientQueue fv [SettingsMessage settings]
            res' <- fetchFriends (accScreenName settings) cfg
            case res' of
                Left err' -> putToClientQueue fv [ ErrorMessage err' ]
                Right fs  -> putToClientQueue fv [ FriendsListMessage fs ]

    where
    fetchSettings :: Url -> Cfg -> IO (Either JsonApiError TASettings)
    fetchSettings url cfg = do
        req <- parseUrl url

        res <- try $ withManager $ \m -> do
                 signedreq <- signOAuth (oauthToken cfg) (oauthCredential cfg) req
                 httpLbs signedreq m

        case res of
            Left e  -> return $ Left $ JsonApiError "" (pack $ show (e :: HttpException))
            Right r -> case eitherDecode $ Network.HTTP.Conduit.responseBody r of
                Left msg -> return $ Left $ JsonApiError "" $ pack msg
                Right s  -> return $ Right s

    fetchFriends :: ScreenName -> Cfg -> IO (Either JsonApiError [User])
    fetchFriends sn cfg = do
        -- TODO handle exceptions
        res <- withManager $ \mgr ->
            sourceWithCursor (twInfo cfg) mgr (friendsList (ScreenNameParam $ unpack sn)) $$ CL.consume


        return $ Right res

--         case res of
--             Left err -> return $ Left $ JsonApiError "" (decodeUtf8 err)
--             Right fs -> return $ Right fs

--         res <- callWithResponse twInfo mgr $ friendsList (ScreenNameParam $ unpack sn)
--         case Web.Twitter.Conduit.responseStatus res of
--             HTTP.Status {statusCode = 200, statusMessage = _ } ->
--                 return $ Right $ contents $ Web.Twitter.Conduit.responseBody res -- res :: [User]
--
--             HTTP.Status {statusCode=code, statusMessage=msg} ->
--                 return $ Left $ JsonApiError "" (decodeUtf8 msg)

writeApi :: Url -> Cfg -> IO (Either (ApiError HttpException) FeedMessage)
writeApi url cfg = do
    req <- parseUrl url
    let req' = req { method = "POST" }
    res <- try $ withManager $ \m -> do
                 signedreq <- signOAuth (oauthToken cfg) (oauthCredential cfg) req'
                 httpLbs signedreq m

    case res of
        Left (StatusCodeException (HTTP.Status code msg) _ _)  ->
            return $ Left $ ApiError $ "Twitter API returned bad status code: " ++ show code ++ " " ++ show msg

        Left e -> return $ Left $ ApiError $ "Other status exception: " ++ show e

        Right r -> case eitherDecode $ Network.HTTP.Conduit.responseBody r of
            Left msg -> return $ Left $ ApiError msg
            Right t  -> return $ Right $ TweetMessage t

readApi :: Feed -> Cfg -> IO (Feed, Either (ApiError HttpException) [Tweet])
readApi feed cfg = do
  req <- parseUrl $ unfeedUrl feed
  res <- try $ withManager $ \m -> do
             signedreq <- signOAuth (oauthToken cfg) (oauthCredential cfg) req
             httpLbs signedreq m

  case res of
    Left (StatusCodeException (HTTP.Status code msg) _ _) ->
      return (feed, Left $ ApiError $ "Twitter API returned bad status code: " ++ show code ++ " " ++ show msg)
    Left x ->
      return (feed, Left $ TransportError x)

    Right r -> case eitherDecode $ Network.HTTP.Conduit.responseBody r of
      Left msg -> return (feed, Left $ ApiError msg)
      Right ts -> return (feed, Right ts)

  where
    unfeedUrl :: Feed -> Url
    unfeedUrl (UserTimeline u) = u
    unfeedUrl (HomeTimeline u) = u

getRunTime :: UTCTime -> IO NominalDiffTime
getRunTime st = do
    endTime <- getCurrentTime
    return $ diffUTCTime endTime st

-- TODO type aliases and type for status
getStatus :: UTCTime -> DL.MyDb -> Cfg -> IO (TweetId, UTCTime, NominalDiffTime)
getStatus st db cfg = do
    xs <- CDL.readCloudDb (cfgCloudDbUrl cfg)
    (_, prevTime) <- DL.getPrevState db
    rt <- getRunTime st

    let z = case xs of
              Left _   -> -1 -- unknown or error
              Right [] -> -2 -- cloud db empty
              Right y  -> CDL.lastSeenId (maximum y)

    return (z, prevTime, rt)
