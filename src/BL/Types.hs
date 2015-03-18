{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module BL.Types where

import           Control.Applicative
import           Control.Concurrent     (MVar, ThreadId)
import           Control.Exception.Base
import           Control.Monad
import           Data.Aeson
import           Data.ByteString
import           Data.Int               (Int64)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime (..))
import           GHC.Generics
import           Web.Twitter.Types      (User (..))

type Url = String
type Username = String
type ScreenName = Text
type TweetId = Int64
type TweetBody = ByteString

data Message = Message Int

data IPCMessage = MReloadFeed | MExit | MOther | MNOOP deriving Show

data FeedMessage = TweetMessage Tweet | UserMessage User deriving (Show, Generic)

instance Show (MVar IPCMessage)
instance Show (MVar [FeedMessage])

type FeedState = [FeedMessage]

type UpdateMessage = UTCTime
instance Show (MVar UTCTime)


makeAppState :: UTCTime -> a -> Maybe ThreadId -> Maybe ThreadId -> Maybe ThreadId -> Maybe ThreadId -> MVar FeedState -> MVar IPCMessage -> MVar UpdateMessage -> AppState a
makeAppState st db x y z u fv av uv = RunState st db x y z u fv av uv

data AppState a = RunState { startTime       :: UTCTime
                           , db              :: a
                           , timeoutWorkerId :: Maybe ThreadId
                           , streamWorkerId  :: Maybe ThreadId
                           , uiWorkerId      :: Maybe ThreadId
                           , updateWorkerId  :: Maybe ThreadId
                           , feedVar         :: MVar [FeedMessage]
                           , appBusVar       :: MVar IPCMessage
                           , updateVar       :: MVar UpdateMessage
                           } deriving Show

data Feed = UserTimeline Url
          | HomeTimeline Url
          deriving Show

data TweetElement = AtUsername String
                  | Link String
                  | PlainText String
                  | Hashtag String
                  | Retweet
                  | Spaces String
                  | Unparsable String
                  deriving Show

data Tweet = Tweet { text       :: [TweetElement]
                   , created_at :: Text
                   , id_        :: TweetId
                   , id_str     :: String
                   , user       :: Author
                   , entities   :: Entities
                   , retweet    :: Maybe Tweet
                   } deriving (Show, Generic)

data Entities = Entities { urls     :: [EntityUrl]
                         , hashtags :: [EntityHashtag]
                         , media    :: Maybe [EntityMedia]
                         } deriving (Show, Generic)

data EntityUrl = EntityUrl { eExpandedUrl :: Url
                           , eUrl         :: Url
                           , eIndices     :: [Int]
                           , eDisplayUrl  :: String
                           } deriving (Show)

data EntityHashtag = EntityHashtag { hText    :: Text
                                   , hIndices :: [Int]
                                   } deriving (Show)

data EntityMedia = EntityMedia { mType        :: String
                               , mIndices     :: [Int]
                               , mUrl         :: Url
                               , mMediaUrl    :: Url
                               , mDisplayUrl  :: String
                               , mExpandedUrl :: Url
                               , mSizes       :: EntityMediaSizes
                               } deriving (Show)

data EntityMediaSize = EntityMediaSize { h      :: Int
                                       , w      :: Int
                                       , resize :: String
                                       } deriving (Show, Generic)

data EntityMediaSizes = EntityMediaSizes { thumb  :: EntityMediaSize
                                         , large  :: EntityMediaSize
                                         , medium :: EntityMediaSize
                                         , small  :: EntityMediaSize
                                         } deriving (Show, Generic)

data Author = Author { name                  :: Text
                     , authorId              :: Integer
                     , screen_name           :: Text
                     , default_profile_image :: Bool
                     , profile_image_url     :: Url
                     } deriving (Show, Eq, Ord)

data JsonApiError = JsonApiError { errTitle   :: Text
                                 , errMessage :: Text
                                 } deriving (Show, Generic)

data JsonResponse = JsonResponse { okTitle        :: Text
                                 , okFeedMessages :: FeedState
                                 } deriving (Show, Generic)

data JsonUserInfo = JsonUserInfo { uiTitle :: Text
                                 , uiData  :: User
                                 } deriving (Show, Generic)

data Exception a => ApiError a = ApiError String | TransportError a deriving Show

instance Eq Tweet where
  (Tweet _ _ aid _ _ _ _) == (Tweet _ _ bid _ _ _ _) = aid == bid

instance Ord Tweet where
   max x@(Tweet _ _ aid _ _ _ _) y@(Tweet _ _ bid _ _ _ _) = if aid >= bid then x else y
   (Tweet _ _ aid _ _ _ _) <= (Tweet _ _ bid _ _ _ _) = aid <= bid

data JsonUnreadCount = JsonUnreadCount  { unreadCount :: Int } deriving (Show, Generic)
