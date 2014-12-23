{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module BL.Worker where

import Control.Concurrent
import Control.Monad (forever)

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit as HTTP
import Web.Authenticate.OAuth
import qualified Data.ByteString.Char8 as BS

import qualified BL.Core as BLC --       (saveFeed, getHomeFeed, readApi, homeTimelineSince, homeTimeline)
import BL.DataLayer  (MyDb, getPrevState)
import BL.Types
import qualified Config as CFG

getFeedUrl :: TweetId -> Feed
getFeedUrl x | x == 0 = BLC.homeTimeline 0
getFeedUrl x | x > 0 = BLC.homeTimelineSince x

getMaxId :: [Tweet] -> TweetId -> TweetId
getMaxId [] oldMaxAvailableId = oldMaxAvailableId
getMaxId ts _ = BL.Types.id_ $ maximum ts


pollWorker :: MyDb -> MVar [Tweet] -> Int -> IO ThreadId
pollWorker db m delay = forkIO $ do
    forever $ do
        (_, oldMaxAvailableId, oldCountNew) <- getPrevState db
        let feedUrl = getFeedUrl oldMaxAvailableId

        print $ "<<<< worker read api " ++ show feedUrl

        (feed, res) <- BLC.readApi feedUrl

        case res of
            Left err ->
              print $ show err

            Right ts -> do
              print $ ">>>> got new data: newMaxAvailableId " ++ show newMaxAvailableId ++ ", newCountNew " ++ show newCountNew
              BLC.saveFeed db newMaxAvailableId newCountNew
              putMVar m ts
              where
                newMaxAvailableId = getMaxId ts oldMaxAvailableId
                newCountNew = length ts

        threadDelay delay

streamWorker :: MyDb -> MVar [Tweet] -> IO ThreadId
streamWorker db m = forkIO $ do
  withManager$ \mgr ->do
    src <- stream twInfo mgr userstream
    src C.$$+- CL.mapM_ (^! act (liftIO . handleTL))

  where
    twInfo :: TWInfo
    twInfo = setCredential tokens credential def

    credential :: Credential
    credential = Credential [ ("oauth_token", BS.pack CFG.accessToken)
                            , ("oauth_token_secret", BS.pack CFG.accessTokenSecret) ]

    tokens :: OAuth
    tokens = twitterOAuth { oauthConsumerKey = BS.pack CFG.oauthConsumerKey
                          , oauthConsumerSecret = BS.pack CFG.oauthConsumerSecret }

    handleTL :: StreamingAPI -> IO ()
    handleTL (SStatus s) = handleTweet $ BLC.statusToTweet s
    handleTL (SRetweetedStatus s) = handleTweet $ BLC.retweetStatusToTweet s

    handleTL s = print $ "???? got not a tweet " ++ show s

    handleTweet t = do
        print $ "!!!! got tweet " ++ show t

        (_, oldMaxAvailableId, oldCountNew) <- getPrevState db
        BLC.saveFeed db (getMaxId [t] oldMaxAvailableId) 1
        putMVar m [t]
--        putMVar m $ Message (oldCountNew + 1)