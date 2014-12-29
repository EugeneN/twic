module Types where

import Data.Maybe
import Control.Monad.Eff (Eff(..))

type TweetId = Number
type Url = String

-- class AppModule a where
--     start :: forall a eff. a -> AppState -> AppModuleArgs -> Eff eff Unit
--     stop  :: forall a eff. a -> Eff eff Unit

-- data AppModuleArgs = {}

newtype OldFeed     = OldFeed [Tweet]
newtype CurrentFeed = CurrentFeed [Tweet]
newtype NewFeed     = NewFeed [Tweet]

data State = State { oldFeed     :: OldFeed
                   , currentFeed :: CurrentFeed
                   , newFeed     :: NewFeed }


data TweetElement = AtUsername String
                  | Link String
                  | PlainText String
                  | Hashtag String
                  | Retweet String
                  | Spaces String
                  | Unparsable String

data Author = Author { name                  :: String
                     , authorId              :: Number
                     , screen_name           :: String
                     , default_profile_image :: Boolean
                     , profile_image_url     :: Url
                     }

data Tweet = Tweet { text       :: [TweetElement]
                   , created_at :: String
                   , id         :: TweetId
                   , id_str     :: String
                   , user       :: Author
                   , entities   :: Entities
                   , retweet    :: Maybe Tweet
                   }

data Entities = Entities { urls     :: [EntityUrl]
                         , hashtags :: [EntityHashtag]
                         , media    :: Maybe [EntityMedia]
                         }

data EntityUrl = EntityUrl { eExpandedUrl :: Url
                           , eUrl         :: Url
                           , eIndices     :: [Number]
                           }

data EntityHashtag = EntityHashtag { hText    :: String
                                   , hIndices :: [Number]
                                   }

data EntityMedia = EntityMedia { mType        :: String
                               , mIndices     :: [Number]
                               , mUrl         :: Url
                               , mMediaUrl    :: Url
                               , mDisplayUrl  :: String
                               , mExpandedUrl :: Url
                               , mSizes       :: EntityMediaSizes
                               }

data EntityMediaSize = EntityMediaSize { h :: Number
                                       , w :: Number
                                       , resize :: String
                                       }

data EntityMediaSizes = EntityMediaSizes { thumb  :: EntityMediaSize
                                         , large  :: EntityMediaSize
                                         , medium :: EntityMediaSize
                                         , small  :: EntityMediaSize
                                         }

type AjaxResult = String

data Observer = Observer { ok :: forall e. AjaxResult -> Eff e Unit
                         , nok :: Maybe (forall e. AjaxResult -> Eff e Unit) }


data ApiResponse  = ResponseSuccess { okTitle    :: String
                                    , okTweets   :: [Tweet]
                                    }

                  | ResponseError { errTitle    :: String
                                  , errMessage  :: String
                                  }

                  | Timeout { toTitle   :: String
                            , toMessage :: String
                            }

data CheckResponse = CheckResponse { unreadTitle :: String
                                   , unreadCount :: Number }

