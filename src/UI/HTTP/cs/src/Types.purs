module Types where

import Data.Maybe
import Data.Monoid
import Control.Monad.Eff (Eff(..))
import Data.Set
import Prelude

type TweetId = Number
type TweetIdS = String
type Url = String

-- class AppModule a where
--     start :: forall a eff. a -> AppState -> AppModuleArgs -> Eff eff Unit
--     stop  :: forall a eff. a -> Eff eff Unit

-- data AppModuleArgs = {}

foreign import data UUID :: *
foreign import data UUIDEff :: !

data Account = Account { settings :: Maybe TASettings
                       , friends  :: Maybe (Set User) }

data TASettings = TASettings { accScreenName :: String }

data FeedMessage = TweetMessage Tweet
                 | UserMessage User
                 | SettingsMessage TASettings
                 | FriendsListMessage Array User
                 | ErrorMessage ApiResponse

data OldFeed a     = OldFeed (Array a)
data CurrentFeed a = CurrentFeed (Array a)
data NewFeed a     = NewFeed (Array a)

instance semigroupNewFeed :: Semigroup (NewFeed a) where
    (<>) (NewFeed as) (NewFeed bs) = NewFeed $ as <> bs

instance semigroupOldFeed :: Semigroup (OldFeed a) where
    (<>) (OldFeed as) (OldFeed bs) = OldFeed $ as <> bs

instance semigroupCurrentFeed :: Semigroup (CurrentFeed a) where
    (<>) (CurrentFeed as) (CurrentFeed bs) = CurrentFeed $ as <> bs


instance functorNewFeed :: Functor NewFeed where
    (<$>) f (NewFeed xs) = NewFeed $ f <$> xs

instance functorOldFeed :: Functor OldFeed where
    (<$>) f (OldFeed xs) = OldFeed $ f <$> xs

instance functorCurrentFeed :: Functor CurrentFeed where
    (<$>) f (CurrentFeed xs) = CurrentFeed $ f <$> xs


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

data AFeed = AFeed { oldFeed     :: OldFeed Tweet
                   , currentFeed :: CurrentFeed Tweet
                   , newFeed     :: NewFeed Tweet }

data BFeed = BFeed { oldFeed     :: OldFeed Tweet
                   , currentFeed :: CurrentFeed Tweet
                   , newFeed     :: NewFeed Tweet
                   , author      :: Maybe Author }

data FriendUser = FriendUser { friendUserId :: UserId
                             , friendUser   :: Maybe User }

data MyInfo = MyInfo { userInfo :: Maybe User
                     , friends  :: Array FriendUser
                     , visible  :: Boolean }

data State = State { feed        :: AFeed
                   , extraFeed   :: Maybe BFeed -- TODO use smth like ExtraFeed = UserFeed{} | SearchFeed {} | etc
                   , errors      :: Array StatusMessage
                   , contextMenu :: ContextMenu
                   , writeInput  :: WriteInput
                   , userInfo    :: UserInfo
                   , myInfo      :: MyInfo
                   , searchInput :: SearchInput
                   , account     :: Account
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

data Tweet = Tweet { text       :: Array TweetElement
                   , created_at :: String
                   , id         :: TweetId
                   , id_str     :: String
                   , user       :: Author
                   , entities   :: Entities
                   , retweet    :: Maybe Tweet
                   , favorited  :: Boolean
                   , retweeted  :: Boolean
                   }

data Entities = Entities { urls     :: Array EntityUrl
                         , hashtags :: Array EntityHashtag
                         , media    :: Maybe (Array EntityMedia)
                         }

data EntityUrl = EntityUrl { eExpandedUrl :: Url
                           , eUrl         :: Url
                           , eIndices     :: Array Number
                           }

data EntityHashtag = EntityHashtag { hText    :: String
                                   , hIndices :: Array Number
                                   }

data EntityMedia = EntityMedia { mType        :: String
                               , mIndices     :: Array Number
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

instance userOrd :: Ord User where
    compare (User {userId=a}) (User {userId=b}) =
        if a > b then GT
        else if a < b then LT
                      else EQ

instance userEq :: Eq User where
  (==) (User {userId=a}) (User {userId=b}) = a == b
  (/=) (User {userId=a}) (User {userId=b}) = a /= b

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
                                    , okFeedMessages  :: Array FeedMessage }

                  | ResponseError { errTitle    :: String
                                  , errMessage  :: String }

                  | ResponseTimeout { toTitle   :: String
                                    , toMessage :: String }

                  | ResponseUserinfo { uiTitle  :: String
                                     , uiData   :: User }

data CheckResponse = CheckResponse { unreadTitle :: String
                                   , unreadCount :: Number }
