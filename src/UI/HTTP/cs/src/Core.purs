module Core where

import Debug.Trace
import Control.Monad.Eff (Eff(..))
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined

import qualified Network.XHR as X
import qualified Network.XHR.Internal as XI
import qualified Network.XHR.Types as XT

import Data.Foreign
import Data.Foreign.Class
import Data.Either
import Data.Monoid
import Data.Maybe

import Utils
import Types


data TweetElement = AtUsername String
                  | Link String
                  | PlainText String
                  | Hashtag String
                  | Retweet String
                  | Spaces String
                  | Unparsable String

toTweetToken :: String -> String -> TweetElement
toTweetToken "AtUsername"   = AtUsername
toTweetToken "Link"         = Link
toTweetToken "PlainText"    = PlainText
toTweetToken "Hashtag"      = Hashtag
toTweetToken "Spaces"       = Spaces
toTweetToken "Unparsable"   = Unparsable

instance isForeignTweetElement :: IsForeign TweetElement where
    read data_ = do
        k <- readProp "type" data_
        v <- readProp "value" data_

        return $ toTweetToken k v


data Author = Author { name                  :: String
                     , authorId              :: Number
                     , screen_name           :: String
                     , default_profile_image :: Boolean
                     , profile_image_url     :: Url
                     }

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

data Tweet = Tweet { text       :: [TweetElement]
                   , created_at :: String
                   , id         :: TweetId
                   , id_str     :: String
                   , user       :: Author
                   , entities   :: Entities
                   , retweet    :: Maybe Tweet
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

        return $ Tweet { text: t
                       , created_at: c
                       , id: i
                       , id_str: s
                       , user: a
                       , entities: e
                       , retweet: r
                       }

data ApiResponse  = ResponseSuccess { okTitle    :: String
                                    , okTweets   :: [Tweet]
                                    }

                  | ResponseError { errTitle    :: String
                                  , errMessage  :: String
                                  }

                  | Timeout { toTitle   :: String
                            , toMessage :: String
                            }

instance isForeignResponseError :: IsForeign ApiResponse
    where
        read data_ =
            case (readProp "errTitle" data_) of
              Left err -> case (readProp "okTitle" data_) of
                Left err' ->
                  return $ ResponseError {errTitle: "Other error", errMessage: "Can't parse response 1"}

                Right t' -> do
                  ts <- readProp "okTweets" data_
                  return $ ResponseSuccess {okTitle: t', okTweets: ts}

              Right t -> do
                m <- readProp "errMessage" data_
                return $ ResponseError {errTitle: t, errMessage: m}

data CheckResponse = CheckResponse { unreadTitle :: String
                                   , unreadCount :: Number }

instance isForeignCheckResponse :: IsForeign CheckResponse where
    read x = do
        c <- "unreadCount" `readProp` x

        return $ CheckResponse { unreadTitle: (show c) ++ " new tweets", unreadCount: c }

data Entities = Entities { urls     :: [EntityUrl]
                         , hashtags :: [EntityHashtag]
                         , media    :: Maybe [EntityMedia]
                         }

instance isForeignEntities :: IsForeign Entities where
  read x = do
    u <- "urls" `readProp` x
    h <- "hashtags" `readProp` x
    m <- runNullOrUndefined <$> "media" `readProp` x

    return $ Entities { urls: u, hashtags: h, media: m }

data EntityUrl = EntityUrl { eExpandedUrl :: Url
                           , eUrl         :: Url
                           , eIndices     :: [Number]
                           }

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

data EntityHashtag = EntityHashtag { hText    :: String
                                   , hIndices :: [Number]
                                   }

instance isForeignHashtag :: IsForeign EntityHashtag where
  read x = do
    t <- "text" `readProp` x
    i <- "indices" `readProp` x

    return $ EntityHashtag { hText: t, hIndices: i }

data EntityMedia = EntityMedia { mType        :: String
                               , mIndices     :: [Number]
                               , mUrl         :: Url
                               , mMediaUrl    :: Url
                               , mDisplayUrl  :: String
                               , mExpandedUrl :: Url
                               , mSizes       :: EntityMediaSizes
                               }

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

data EntityMediaSize = EntityMediaSize { h :: Number
                                       , w :: Number
                                       , resize :: String
                                       }

instance isForeignMediaSize :: IsForeign EntityMediaSize where
  read x = do
    h <- "h" `readProp` x
    w <- "w" `readProp` x
    resize <- "resize" `readProp` x

    return $ EntityMediaSize { h: h, w: w, resize: resize}

data EntityMediaSizes = EntityMediaSizes { thumb  :: EntityMediaSize
                                         , large  :: EntityMediaSize
                                         , medium :: EntityMediaSize
                                         , small  :: EntityMediaSize
                                         }

instance isForeignMediaSizes :: IsForeign EntityMediaSizes where
  read x = do
    thumb   <- "thumb" `readProp` x
    large   <- "large" `readProp` x
    medium  <- "medium" `readProp` x
    small   <- "small" `readProp` x

    return $ EntityMediaSizes {thumb: thumb, large: large, medium: medium, small: small}


instance showResponse :: Show X.Response where
    show = toString

retweet :: forall e. String -> Eff (trace :: Trace, ajax :: XI.Ajax | e) Unit
retweet id_ = do
    let url = "/retweet/?id=" ++ id_

    trace $ "retweet " ++ url

    X.post X.defaultAjaxOptions
        { onReadyStateChange = X.onDone \resp -> do
          trace $ "retweeted " ++ show resp
          pure unit
        } url {} (XT.UrlEncoded "")

    pure unit

fromResponse :: String -> ApiResponse
fromResponse x = case (readJSON x :: F ApiResponse) of
  Left err -> ResponseError {errTitle: "Other error", errMessage: "Can't parse response"}
  Right resp -> resp


fromCheckResponse :: String -> CheckResponse
fromCheckResponse x = case (readJSON x :: F CheckResponse) of
    Left err -> CheckResponse { unreadTitle: "Check failed", unreadCount: -1 }
    Right resp -> resp

