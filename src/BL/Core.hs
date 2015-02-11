{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BL.Core (
    Url
  , Tweet(..)
  , Author(..)
  , Username
  , ApiError(..)
  , Entities(..)
  , EntityUrl(..)
  , EntityMedia(..)
  , TweetElement(..)
  , JsonApiError(..)
  , JsonResponse(..)
  , TweetId
  , retweetStatusToTweet
  , handleIncomingTweets
  , statusToTweet
  , saveLastSeen
  , saveLastSeenAsync
  , updateFeed
  , updateFeedSync
  , retweetUrl
  , tweetUrl
  , writeApi
  , readApi
  , starUrl
  ) where

import           Data.Text                 (pack, unpack)

import           System.IO
import           Web.Authenticate.OAuth

import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as B8
import           Network.HTTP.Conduit

import           Control.Exception.Lifted  (try)

import           Control.Applicative
import           Control.Concurrent        (MVar, ThreadId, forkIO, killThread,
                                            modifyMVar_, newEmptyMVar, newMVar,
                                            putMVar, readMVar, swapMVar,
                                            takeMVar, threadDelay, tryTakeMVar)
import           Control.Monad
import           Control.Monad.Trans       (liftIO)
import           Network.HTTP.Types        (Status (..))

import qualified BL.CloudDataLayer         as CDL
import qualified BL.DataLayer              as DL
import           BL.Parser                 (parseTweet)
import           BL.Types
import qualified Config                    as CFG
import           Data.Aeson
import           Data.HashMap.Strict
import           Data.Int                  (Int64)
import           Data.Time.Clock           (UTCTime (..), diffUTCTime,
                                            getCurrentTime, secondsToDiffTime)
import           GHC.Generics
import           Prelude                   hiding (error)
import           System.Log.Handler.Simple
import           System.Log.Logger
import qualified Web.Twitter.Types         as TT

logRealm = "Core"

info = infoM logRealm
warn = warningM logRealm
debug = debugM logRealm
error = errorM logRealm

myoauth :: OAuth
myoauth = newOAuth { oauthServerName     = CFG.serverName
                   , oauthConsumerKey    = B8.pack CFG.oauthConsumerKey
                   , oauthConsumerSecret = B8.pack CFG.oauthConsumerSecret
                   }

mycred :: Credential
mycred = newCredential (B8.pack CFG.accessToken) (B8.pack CFG.accessTokenSecret)


retweetStatusToTweet :: TT.RetweetedStatus -> Tweet
retweetStatusToTweet s = Tweet (parseTweet $ TT.rsText s)
                               (pack $ TT.rsCreatedAt s)
                               (fromIntegral (TT.rsId s) :: Int64)
                               (show $ TT.rsId s)
                               (statusUserToAuthor $ TT.rsUser s)
                               (statusEntitiesToEntities $ TT.rsEntities s)
                               (Just $ statusToTweet $ TT.rsRetweetedStatus s)

statusRetweetToRetweet :: Maybe TT.Status -> Maybe Tweet
statusToTweet :: TT.Status -> Tweet
statusToTweet s = Tweet (parseTweet $ TT.statusText s)
                        (pack $ TT.statusCreatedAt s)
                        (fromIntegral (TT.statusId s) :: Int64)
                        (show $ TT.statusId s)
                        (statusUserToAuthor $ TT.statusUser s)
                        (statusEntitiesToEntities $ TT.statusEntities s)
                        (statusRetweetToRetweet $ TT.statusRetweetedStatus s)

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


statusEntitiesToEntities :: Maybe TT.Entities -> Entities
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

    return $ Tweet (parseTweet text) created_at id_ id_str user entities retweet

  parseJSON _ = fail "tweet is expected to be an object"

instance ToJSON Tweet where
  toJSON x = object [ "text"       .= text x
                    , "created_at" .= created_at x
                    , "id"         .= BL.Types.id_ x
                    , "id_str"     .= id_str x
                    , "user"       .= user x
                    , "entities"   .= entities x
                    , "retweet"    .= retweet x
                    ]

instance FromJSON JsonApiError
instance ToJSON JsonApiError

instance FromJSON JsonResponse
instance ToJSON JsonResponse

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

retweetUrl :: TweetId -> Url
retweetUrl x = "https://api.twitter.com/1.1/statuses/retweet/" ++ show x ++ ".json"

starUrl :: TweetId -> Url
starUrl x = "https://api.twitter.com/1.1/favorites/create.json?id=" ++ show x

tweetUrl :: TweetBody -> Url
tweetUrl status = "https://api.twitter.com/1.1/statuses/update.json?status=" ++ B8.unpack status

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


getUpdateFeedUrl :: DL.MyDb -> IO Feed
getUpdateFeedUrl db = do
  xs <- CDL.readCloudDb
  info $ "Read from cloud db with result of length: " ++ show (length <$> xs)

  let z = case xs of
          Left x  -> 0
          Right y -> CDL.lastSeenId (maximum y)

  debug $ "Selected max tweet id: " ++ show z
  return $ homeTimelineSince z

--------------------------------------------------------------------------------

saveLastSeenAsync :: DL.MyDb -> [Tweet] -> IO ThreadId
saveLastSeenAsync db ts = forkIO $ saveLastSeen db ts >> return ()

saveLastSeen :: DL.MyDb -> [Tweet] -> IO (Either CDL.CloudDataLayerError CDL.WriteResponse)
saveLastSeen db ts = do
    now <- getCurrentTime
    res <- CDL.writeCloudDb $ CDL.CloudDbStoreItem (getMaxId ts) (show now) -- now
    info $ "Wrote to cloud db with result: " ++ show res
    return res

    where
    getMaxId :: [Tweet] -> TweetId
    getMaxId ts = maximum (BL.Types.id_ <$> ts)

handleIncomingTweets :: DL.MyDb -> MVar [Tweet] -> [Tweet] -> IO ()
handleIncomingTweets _ _ [] = do
    info "Got no updated tweets, skipping saving"
    return ()
handleIncomingTweets _ fv ts = do
    info $ "Got " ++ show (length ts) ++ " updated tweet/s"
    maybeOldTs <- tryTakeMVar fv
    case maybeOldTs of
        Just oldts -> putMVar fv $ oldts ++ ts
        Nothing    -> putMVar fv ts


updateFeed :: MVar UTCTime -> IO ()
updateFeed uv = do
    now <- getCurrentTime
    debug $ "***Putting an update request at " ++ show now
    _ <- forkIO $ putMVar uv now -- avoid blocking caller
    return ()

updateFeedSync :: DL.MyDb -> MVar [Tweet] -> IO ()
updateFeedSync db fv = do
    info "Updating feed"
    feedUrl <- getUpdateFeedUrl db
    doreq feedUrl db fv (0 :: Int)

  where
  doreq f db fv iter = do
    (_, res) <- readApi f
    case res of
      Right ts -> handleIncomingTweets db fv (reverse ts)

      Left (TransportError (FailedConnectionException2 _ _ _ ex)) -> if iter < CFG.updateRetryCount
        then do
          error $ "Http error at update attempt " ++ show iter ++ ": " ++ show ex
                ++ ". Retrying in " ++ show CFG.updateRetryDelay ++ "ms"
          threadDelay CFG.updateRetryDelay
          doreq f db fv (iter + 1)

        else
          error $ "error: update retry count exceeded: error was: " ++ show ex

      Left (TransportError ex) -> error $ "error: transport error: " ++ show ex
      Left (ApiError msg) -> error $ "error: api error: " ++ show msg

writeApi :: Url -> IO (Either (ApiError HttpException) Tweet)
writeApi url = do
    req <- parseUrl url
    let req' = req { method = "POST" }
    res <- try $ withManager $ \m -> do
                 signedreq <- signOAuth myoauth mycred req'
                 httpLbs signedreq m

    case res of
        Left (StatusCodeException (Network.HTTP.Types.Status code msg) _ _)  ->
            return $ Left $ ApiError $ "Twitter API returned bad status code: " ++ show code ++ " " ++ show msg

        Left e -> return $ Left $ ApiError $ "Other status exception: " ++ show e

        Right r -> case eitherDecode $ responseBody r of
            Left msg -> return $ Left $ ApiError msg
            Right t  -> return $ Right t

readApi :: Feed -> IO (Feed, Either (ApiError HttpException) [Tweet])
readApi feed = do
  req <- parseUrl $ unfeedUrl feed
  res <- try $ withManager $ \m -> do
             signedreq <- signOAuth myoauth mycred req
             httpLbs signedreq m

  case res of
    Left (StatusCodeException (Network.HTTP.Types.Status code msg) _ _) ->
      return (feed, Left $ ApiError $ "Twitter API returned bad status code: " ++ show code ++ " " ++ show msg)
    Left x ->
      return (feed, Left $ TransportError x)

    Right r -> case eitherDecode $ responseBody r of
      Left msg -> return (feed, Left $ ApiError msg)
      Right ts -> return (feed, Right ts)

  where
    unfeedUrl :: Feed -> Url
    unfeedUrl (UserTimeline u) = u
    unfeedUrl (HomeTimeline u) = u


