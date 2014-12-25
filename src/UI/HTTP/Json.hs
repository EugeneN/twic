{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.Json where

import           BL.Core
import           BL.Types                            (JsonUnreadCount(..))
import           Debug.Trace                         (trace)
import qualified Data.ByteString.Lazy                as B
import           Data.Aeson
import qualified Data.Text                           as T
import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Blaze.ByteString.Builder            (Builder)



justTweetsToJson :: Either ApiError [Tweet] -> Builder
justTweetsToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode $ JsonApiError {errTitle = "Error", errMessage = T.pack msg}

justTweetsToJson (Right []) =
    fromLazyByteString $ encode $ JsonResponse {okTweets = [], okTitle = "No new tweets"}

justTweetsToJson (Right ts) =
    fromLazyByteString $ encode $ JsonResponse {okTweets = ts, okTitle = T.pack $ (show $ length ts) ++ " new tweets"}


justUnreadCountToJson :: Int -> Builder
justUnreadCountToJson n = fromLazyByteString $ encode $ JsonUnreadCount {unreadCount = n}