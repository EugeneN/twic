{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BL.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString
import           Data.Int            (Int64)
import           Data.Text           (Text)
import           GHC.Generics

type Url = String
type Username = String
type TweetId = Int64
type TweetBody = ByteString

data Message = Message Int

data IPCMessage = MReloadFeed | MOther | MNOOP deriving Show

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

data JsonResponse = JsonResponse { okTitle  :: Text
                                 , okTweets :: [Tweet]
                                 } deriving (Show, Generic)

data ApiError = ApiError String deriving Show

instance Eq Tweet where
  (Tweet _ _ aid _ _ _ _) == (Tweet _ _ bid _ _ _ _) = aid == bid

instance Ord Tweet where
   max x@(Tweet _ _ aid _ _ _ _) y@(Tweet _ _ bid _ _ _ _) = if aid >= bid then x else y
   (Tweet _ _ aid _ _ _ _) <= (Tweet _ _ bid _ _ _ _) = aid <= bid

data JsonUnreadCount = JsonUnreadCount  { unreadCount :: Int } deriving (Show, Generic)
