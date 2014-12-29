{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module BL.Core (
    Url(..)
  , Tweet(..)
  , Author(..)
  , Username(..)
  , ApiError(..)
  , Entities(..)
  , EntityUrl(..)
  , EntityMedia(..)
  , TweetElement(..)
  , JsonApiError(..)
  , JsonResponse(..)
  , TweetId
  , retweetStatusToTweet
  , homeTimelineSince
  , getUnreadCount
  , saveFeedStatus
  , getCachedFeed
  , statusToTweet
  , homeTimeline
  , getHomeFeed
  , retweetUrl
  , getFeedUrl
  , tweetUrl
  , getMaxId
  , writeApi
  , readApi
  , starUrl
  ) where

import           Data.Text                 (Text, pack, unpack)

import           Web.Authenticate.OAuth
import           System.IO

import           Network.HTTP.Conduit
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as B8

import           Control.Exception.Lifted  (try)

import           Control.Applicative
import           Control.Monad
import           Network.HTTP.Conduit      (HttpException(..))
import           Network.HTTP.Types        (Status(..))
import           Control.Monad.Trans       (liftIO)

import qualified Config                    as CFG
import           BL.Parser                 (parseTweet)
import           BL.Types
import qualified BL.DataLayer              as DL
import           Data.Aeson
import           GHC.Generics
import qualified Web.Twitter.Types         as TT
import           Data.Int                  (Int64)
import           Data.HashMap.Strict


myoauth :: OAuth
myoauth = newOAuth { oauthServerName     = CFG.serverName
                   , oauthConsumerKey    = B8.pack CFG.oauthConsumerKey
                   , oauthConsumerSecret = B8.pack CFG.oauthConsumerSecret
                   }

mycred :: Credential
mycred = newCredential (B8.pack CFG.accessToken) (B8.pack CFG.accessTokenSecret)


retweetStatusToTweet s = Tweet (parseTweet $ TT.rsText s)
                               (pack $ TT.rsCreatedAt s)
                               (fromIntegral (TT.rsId s) :: Int64)
                               (show $ TT.rsId s)
                               (statusUserToAuthor $ TT.rsUser s)
                               (statusEntitiesToEntities $ TT.rsEntities s)
                               (Just $ statusToTweet $ TT.rsRetweetedStatus s)

statusToTweet s = Tweet (parseTweet $ TT.statusText s)
                        (pack $ TT.statusCreatedAt s)
                        (fromIntegral (TT.statusId s) :: Int64)
                        (show $ TT.statusId s)
                        (statusUserToAuthor $ TT.statusUser s)
                        (statusEntitiesToEntities $ TT.statusEntities s)
                        (statusRetweetToRetweet $ TT.statusRetweetedStatus s)

statusUserToAuthor s = Author (TT.userName s)
                              (TT.userId s)
                              (TT.userScreenName s)
                              (TT.userDefaultProfileImage s)
                              (avatarUrl $ TT.userProfileImageURL s)
    where
        avatarUrl x = case x of
            Nothing -> "http://a0.twimg.com/sticky/default_profile_images/default_profile_6_normal.png"
            Just y -> unpack y


--statusEntitiesToEntities Nothing = ?
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

writeApi :: Url -> IO (Either ApiError Tweet)
writeApi url = do
    req <- parseUrl url
    let req' = req { method = "POST" }
    res <- try $ withManager $ \m -> do
                 signedreq <- signOAuth myoauth mycred req'
                 httpLbs signedreq m

    case res of
        Left (StatusCodeException (Network.HTTP.Types.Status code msg) _ _)  ->
            return $ Left $ ApiError $ "Twitter API returned bad status code: " ++ show code ++ " " ++ show msg

        Right r -> case eitherDecode $ responseBody r of
            Left msg -> return $ Left $ ApiError msg
            Right t  -> return $ Right t

readApi :: Feed -> IO (Feed, (Either ApiError [Tweet]))
readApi feed = do
  req <- parseUrl $ unfeedUrl feed
  res <- try $ withManager $ \m -> do
             signedreq <- signOAuth myoauth mycred req
             httpLbs signedreq m

  case res of
    Left (StatusCodeException (Network.HTTP.Types.Status code msg) _ _) ->
      return (feed, (Left $ ApiError $ "Twitter API returned bad status code: " ++ show code ++ " " ++ show msg))
    Left _ ->
      return (feed, (Left $ ApiError "Http error"))

    Right r -> case eitherDecode $ responseBody r of
      Left msg -> return (feed, (Left $ ApiError msg))
      Right ts -> return (feed, Right ts)

  where
    unfeedUrl :: Feed -> Url
    unfeedUrl (UserTimeline u) = u
    unfeedUrl (HomeTimeline u) = u

retweetUrl :: TweetId -> Url
retweetUrl id_ = "https://api.twitter.com/1.1/statuses/retweet/" ++ (show id_) ++ ".json"

starUrl :: TweetId -> Url
starUrl id_ = "https://api.twitter.com/1.1/favorites/create.json?id=" ++ (show id_)

tweetUrl :: TweetBody -> Url
tweetUrl status = "https://api.twitter.com/1.1/statuses/update.json?status=" ++ B8.unpack status

userTimeline :: Username -> Int -> Feed
userTimeline name 0 = UserTimeline $
  "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="++ name
userTimeline name count = UserTimeline $
  "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  ++ name ++ "&count=" ++ (show count)

homeTimeline :: Int -> Feed
homeTimeline 0 = HomeTimeline $ "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimeline count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count

homeTimelineSince :: TweetId -> Feed
homeTimelineSince tid = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?since_id="
  ++ (show tid)

homeTimelineSinceCount :: TweetId -> Int -> Feed
homeTimelineSinceCount 0 0 = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json"
homeTimelineSinceCount tid 0 = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?since_id=" ++ show tid
homeTimelineSinceCount 0 count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?count=" ++ show count
homeTimelineSinceCount tid count = HomeTimeline $
  "https://api.twitter.com/1.1/statuses/home_timeline.json?since_id="
  ++ (show tid) ++ "&count=" ++ (show count)


getCachedFeed :: DL.MyDb -> Int -> IO Feed
getCachedFeed db _ = do
  x <- readCachedFeed db

  case x of
    Just (lastSeenId, countNew) -> return $ homeTimelineSinceCount lastSeenId countNew
    Nothing -> return $ homeTimeline 0

  where
    readCachedFeed :: DL.MyDb -> IO (Maybe (TweetId, Int))
    readCachedFeed db = do
      x <- DL.read db 1
      return $ Just x

getHomeFeed :: DL.MyDb -> Int -> IO Feed
getHomeFeed db n = do return $ homeTimeline n

getUnreadCount :: DL.MyDb -> IO Int
getUnreadCount db = do
   (_, _, n) <- DL.getPrevState db
   return n

getFeedUrl :: TweetId -> Feed
getFeedUrl x | x == 0 = homeTimeline 0
getFeedUrl x | x > 0 = homeTimelineSince x

getMaxId :: [Tweet] -> TweetId -> TweetId
getMaxId [] oldMaxAvailableId = oldMaxAvailableId
getMaxId ts _ = BL.Types.id_ $ maximum ts

saveFeedStatus :: DL.MyDb -> [Tweet] -> IO ()
saveFeedStatus db ts = do
    (oldLastSeenId, oldMaxAvailableId, oldCountNew) <- DL.getPrevState db
    DL.write db (getMaxId ts oldMaxAvailableId) (length ts)