{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.Json where

import           BL.Core
import           BL.Types
import           Blaze.ByteString.Builder            (Builder)
import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Data.Aeson
import qualified Data.ByteString.Lazy                as B
import qualified Data.Text                           as T
import           Debug.Trace                         (trace)
import           Network.HTTP.Conduit                (HttpException)
import           Web.Twitter.Types                   (User)


justFeedMessagesToJson :: Either (ApiError HttpException) FeedState -> Builder
justFeedMessagesToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

justFeedMessagesToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack $ show x}

justFeedMessagesToJson (Right []) =
    fromLazyByteString $ encode JsonResponse {okFeedMessages = [], okTitle = "No new tweets"}

justFeedMessagesToJson (Right ts) =
    fromLazyByteString $ encode JsonResponse {okFeedMessages = ts, okTitle = T.pack $ show (length ts) ++ " new messages"}

--------------------------------------------------------------------------------

justUserToJson :: Either (ApiError HttpException) User -> Builder
justUserToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

justUserToJson (Right user) =
    fromLazyByteString $ encode JsonUserInfo {uiData = user, uiTitle = "userinfo"}

--------------------------------------------------------------------------------

justUserInfoToJson :: Either (ApiError HttpException) User -> Builder
justUserInfoToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

justUserInfoToJson (Right ui) =
    fromLazyByteString $ encode JsonUserInfo {uiData = ui, uiTitle = "userinfo"}

--------------------------------------------------------------------------------

justUnreadCountToJson :: Int -> Builder
justUnreadCountToJson n = fromLazyByteString $ encode JsonUnreadCount {unreadCount = n}

--------------------------------------------------------------------------------

retweetToJson :: Either (ApiError HttpException) FeedMessage -> Builder
retweetToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

retweetToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack $ show x}

retweetToJson (Right t) = fromLazyByteString $ encode t

--------------------------------------------------------------------------------

starToJson :: Either (ApiError HttpException) FeedMessage -> Builder
starToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}
starToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack $ show x}

starToJson (Right t) = fromLazyByteString $ encode JsonResponse {okTitle="ok", okFeedMessages=[t]}

--------------------------------------------------------------------------------

tweetToJson :: Either (ApiError HttpException) FeedMessage -> Builder
tweetToJson (Left (ApiError msg)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack msg}

tweetToJson (Left (TransportError x)) =
    fromLazyByteString $ encode JsonApiError {errTitle = "Error", errMessage = T.pack $ show x}

tweetToJson (Right t) = fromLazyByteString $ encode JsonResponse {okTitle="ok", okFeedMessages=[t]}
