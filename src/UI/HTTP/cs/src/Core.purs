module Core where

import Debug.Trace
import Control.Monad.Eff.Ref
import Control.Monad.Eff (Eff(..))
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import DOM (DOM(..))
import Control.Monad.Eff.Ref
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
import Config

import UI.LoaderIndicator (showLoader, hideLoader)


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

        return $ Tweet { text: t
                       , created_at: c
                       , id: i
                       , id_str: s
                       , user: a
                       , entities: e
                       , retweet: r
                       }

instance isForeignResponseError :: IsForeign ApiResponse
    where
        read data_ =
            case (readProp "errTitle" data_) of
              Left err -> case (readProp "okTitle" data_) of
                Left err' ->
                  return $ ResponseError { errTitle: "Other error"
                                         , errMessage: "Can't parse response 1"}

                Right t' -> do
                  ts <- readProp "okTweets" data_
                  return $ ResponseSuccess {okTitle: t', okTweets: ts}

              Right t -> do
                m <- readProp "errMessage" data_
                return $ ResponseError {errTitle: t, errMessage: m}

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

instance showResponse :: Show X.Response where
    show = toString

--retweet :: forall e eff. String
--                  -> (AjaxResult -> Eff eff Unit)
--                  -> Eff (trace :: Trace, ajax :: XI.Ajax | e) Unit
--retweet id_ retweetResultHandler = do
--    let url = "/retweet/?id=" ++ id_
--
--    trace $ "retweet " ++ url
--
--    (rioPost url Nothing) ~> retweetResultHandler
--
--    pure unit


fromResponse :: String -> ApiResponse
fromResponse x = case (readJSON x :: F ApiResponse) of
  Left err -> ResponseError {errTitle: "Other error", errMessage: "Can't parse response"}
  Right resp -> resp


fromCheckResponse :: String -> CheckResponse
fromCheckResponse x = case (readJSON x :: F CheckResponse) of
    Left err -> CheckResponse { unreadTitle: "Check failed", unreadCount: -1 }
    Right resp -> resp

fromWsMessage :: String -> [Tweet]
fromWsMessage s = case (readJSON s :: F [Tweet]) of
    Left err -> []
    Right ts -> ts

--------------------------------------------------------------------------------


initialState :: State
initialState = State { oldFeed: OldFeed []
                     , currentFeed: CurrentFeed []
                     , newFeed: NewFeed []
                     , historyButtonDisabled: false
                     , errors: [] }

-- TODO get rid of singleton global state
stateObservable = getNewObservable initialState

readState = readRef

writeState state value = do
    writeRef state value
    let x = publishToObservable stateObservable value
    pure unit

foreign import setTitle
    """
    function setTitle(a) {
      return function() {
        document.title = a;
        return undefined;
      }
    }
    """ :: forall eff. String -> Eff (dom :: DOM | eff) Unit








