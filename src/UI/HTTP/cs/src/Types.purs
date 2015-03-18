module Types where

import Data.Maybe
import Data.Monoid
import Control.Monad.Eff (Eff(..))

type TweetId = Number
type TweetIdS = String
type Url = String

-- class AppModule a where
--     start :: forall a eff. a -> AppState -> AppModuleArgs -> Eff eff Unit
--     stop  :: forall a eff. a -> Eff eff Unit

-- data AppModuleArgs = {}

foreign import data UUID :: *
foreign import data UUIDEff :: !

data FeedMessage = TweetMessage Tweet | UserMessage User

newtype OldFeed     = OldFeed [Tweet]
newtype CurrentFeed = CurrentFeed [Tweet]
newtype NewFeed     = NewFeed [Tweet]

instance semigroupNewFeed :: Semigroup NewFeed where
    (<>) (NewFeed as) (NewFeed bs) = NewFeed $ as ++ bs

instance semigroupOldFeed :: Semigroup OldFeed where
    (<>) (OldFeed as) (OldFeed bs) = OldFeed $ as ++ bs

instance semigroupCurrentFeed :: Semigroup CurrentFeed where
    (<>) (CurrentFeed as) (CurrentFeed bs) = CurrentFeed $ as ++ bs

type Msgid = UUID

data StatusMessage = Error   String Msgid
                   | Success String Msgid
                   | Other   String Msgid

data ContextMenu = ContextMenu { visible :: Boolean
                               , x       :: Number
                               , y       :: Number
                               , tweetId :: Maybe TweetIdS
                               }

data SearchInput = SearchInput { visible :: Boolean
                               , value   :: String }

data WriteInput = WriteInput { visible  :: Boolean
                             , disabled :: Boolean
                             , value    :: String
                             , replyTo  :: Maybe Tweet }

data UserInfo = UserInfo { visible :: Boolean
                         , followRequestActive :: Boolean
                         , userdata :: Maybe User }

data AFeed = AFeed { oldFeed     :: OldFeed
                   , currentFeed :: CurrentFeed
                   , newFeed     :: NewFeed }

data BFeed = BFeed { oldFeed     :: OldFeed
                   , currentFeed :: CurrentFeed
                   , newFeed     :: NewFeed
                   , author      :: Maybe Author }

data FriendUser = FriendUser { friendUserId :: UserId
                             , friendUser   :: Maybe User }

data MyInfo = MyInfo { userInfo :: Maybe User
                     , friends  :: [FriendUser]
                     , visible  :: Boolean }

data State = State { feed        :: AFeed
                   , extraFeed   :: Maybe BFeed -- TODO use smth like ExtraFeed = UserFeed{} | SearchFeed {} | etc
                   , errors      :: [StatusMessage]
                   , contextMenu :: ContextMenu
                   , writeInput  :: WriteInput
                   , userInfo    :: UserInfo
                   , myInfo      :: MyInfo
                   , searchInput :: SearchInput
                   , historyButtonDisabled :: Boolean }


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

type UTCTime = String
type UserId = Number
type LanguageCode = String

data User = User { userContributorsEnabled              :: Boolean
                 , userCreatedAt                        :: UTCTime
                 , userDefaultProfile                   :: Boolean
                 , userDefaultProfileImage              :: Boolean
                 , userDescription                      :: Maybe String
                 , userFavoritesCount                   :: Number
                 , userFollowRequestSent                :: Maybe Boolean
                 , userFollowing                        :: Maybe Boolean
                 , userFollowersCount                   :: Number
                 , userFriendsCount                     :: Number
                 , userGeoEnabled                       :: Boolean
                 , userId                               :: UserId
                 , userIsTranslator                     :: Boolean
                 , userLang                             :: LanguageCode
                 , userListedCount                      :: Number
                 , userLocation                         :: Maybe String
                 , userName                             :: String
                 , userNotifications                    :: Maybe Boolean
                 , userProfileBackgroundColor           :: Maybe String
                 , userProfileBackgroundImageURL        :: Maybe Url
                 , userProfileBackgroundImageURLHttps   :: Maybe Url
                 , userProfileBackgroundTile            :: Maybe Boolean
                 , userProfileBannerURL                 :: Maybe Url
                 , userProfileImageURL                  :: Maybe Url
                 , userProfileImageURLHttps             :: Maybe Url
                 , userProfileLinkColor                 :: String
                 , userProfileSidebarBorderColor        :: String
                 , userProfileSidebarFillColor          :: String
                 , userProfileTextColor                 :: String
                 , userProfileUseBackgroundImage        :: Boolean
                 , userProtected                        :: Boolean
                 , userScreenName                       :: String
                 , userShowAllInlineMedia               :: Maybe Boolean
                 , userStatusesCount                    :: Number
                 , userTimeZone                         :: Maybe String
                 , userURL                              :: Maybe Url
                 , userUtcOffset                        :: Maybe Number
                 , userVerified                         :: Boolean
                 , userWithheldInCountries              :: Maybe String
                 , userWithheldScope                    :: Maybe String }

-- TODO rename ResponseSuccess to ResponseFeedMessage
data ApiResponse  = ResponseSuccess { okTitle         :: String
                                    , okFeedMessages  :: [FeedMessage] }

                  | ResponseError { errTitle    :: String
                                  , errMessage  :: String }

                  | ResponseTimeout { toTitle   :: String
                                    , toMessage :: String }

                  | ResponseUserinfo { uiTitle  :: String
                                     , uiData   :: User }

data CheckResponse = CheckResponse { unreadTitle :: String
                                   , unreadCount :: Number }


