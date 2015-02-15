{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module BL.CloudDataLayer where

import           BL.Types                  (TweetId)
import qualified Config                    as CFG
import           Control.Applicative
import           Control.Exception.Base
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString
import qualified Data.ByteString.Lazy      as DBL
import qualified Data.HashMap.Strict       as H
import           Data.Map
import           Data.Time.Clock           (UTCTime (..), getCurrentTime,
                                            secondsToDiffTime)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Prelude                   hiding (error)
import           System.Log.Handler.Simple
import           System.Log.Logger

logRealm = "CloudDataLayer"

info = infoM logRealm
warn = warningM logRealm
debug = debugM logRealm
-- error = errorM logRealm

data CloudDataLayerError
    = CloudDbApiError String
    | CloudDbTransportError HttpException
    | CloudDbDataError String
    deriving (Show)

data WriteResponse = WriteSuccess { name :: String }
                   | WriteError   { error :: String }
                   deriving (Show, Generic)

parseResponse :: Bool -> Bool -> Object -> Parser WriteResponse
parseResponse hasName hasError obj
    | hasName   = WriteSuccess <$> obj .: "name"
    | hasError  = WriteError <$> obj .: "error"
    | otherwise = pure $ WriteError $ "Unknown write response: " ++ show obj

instance FromJSON WriteResponse where
    parseJSON (Object o) = parseResponse hasName hasError o
        where
        hasName = H.member "name" o
        hasError = H.member "error" o

    parseJSON _ = fail "WriteResponse is expected to be an object"

instance ToJSON WriteResponse where
  toJSON (WriteSuccess x) = object [ "name"  .= x ]
  toJSON (WriteError x)   = object [ "error" .= x ]

-- TODO check if tweetid is correctly parsed to/from json
data CloudDbStoreItem = CloudDbStoreItem { lastSeenId :: TweetId
                                         , at         :: String -- UTCTime
                                         } deriving (Show, Eq, Ord, Generic)

instance FromJSON CloudDbStoreItem
instance ToJSON CloudDbStoreItem

type CloudDbStore = [CloudDbStoreItem]

instance FromJSON CloudDbStore
instance ToJSON CloudDbStore

applyLimit :: String -> String -> Int -> String
applyLimit url orderByKey count = url ++ "?orderBy=\"" ++ orderByKey
                                      ++ "\"&limitToLast=" ++ show count

readCloudJSON :: DBL.ByteString -> Either CloudDataLayerError CloudDbStore
readCloudJSON str = case decode str :: Maybe (Map String CloudDbStoreItem) of
    Just y  -> Right (Data.Map.elems y)
    Nothing -> Left $ CloudDbDataError "error reading cloud json"

-- TODO offline garbage collection.
readCloudDb :: IO (Either CloudDataLayerError CloudDbStore)
readCloudDb = do
    req <- parseUrl $ applyLimit CFG.cloudDbUrl "lastSeenId" 10
    res <- try $ withManager $ \m -> httpLbs req m

    return $ case res of
        Left x  -> Left $ CloudDbTransportError x
        Right y -> case readCloudJSON $ responseBody y of
            Left err -> Left err
            Right xs -> Right xs


writeCloudDb :: CloudDbStoreItem -> IO (Either CloudDataLayerError WriteResponse)
writeCloudDb data_ = do
    req <- parseUrl CFG.cloudDbUrl
    let req' = req { method = "POST"
                   , requestBody = RequestBodyLBS $ encode data_
                   , requestHeaders = requestHeaders req
                                   ++ [("Content-Type", "application/json")]
                   }
    res <- try $ withManager $ \m -> httpLbs req' m

    case res of
        Left e  -> return $ Left $ CloudDbTransportError e
        Right r ->
            return $ case eitherDecode (responseBody r) :: Either String WriteResponse of
                Left msg -> Left $ CloudDbDataError $ msg ++ " / " ++ show (responseBody r)
                Right x  -> Right x
