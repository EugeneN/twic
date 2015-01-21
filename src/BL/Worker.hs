{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module BL.Worker where

import Control.Concurrent
import Control.Monad (forever)

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

import Data.Time.Clock (diffUTCTime, getCurrentTime, UTCTime(..), secondsToDiffTime)
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

import qualified BL.Core as BLC
import BL.DataLayer  (MyDb, getPrevState, writeTime)
import BL.Types
import qualified Config as CFG

handleIncomingTweets db m ts = BLC.saveFeedStatus db ts >> putMVar m ts

pollWorker :: MyDb -> MVar [Tweet] -> Int -> IO ThreadId
pollWorker db m delay = forkIO $ do
    forever $ do
        (_, oldMaxAvailableId, _, _) <- getPrevState db
        let feedUrl = BLC.getFeedUrl oldMaxAvailableId

        print $ "<<<< worker read api " ++ show feedUrl

        (feed, res) <- BLC.readApi feedUrl

        case res of
            Left err -> print $ show err
            Right ts -> handleIncomingTweets db m ts

        threadDelay delay

shouldForceUpdate :: UTCTime -> UTCTime -> Bool
shouldForceUpdate prev cur =
  if (diffUTCTime cur prev) < (realToFrac CFG.timeoutThreshod) then False else True

timeoutWorker :: MyDb -> MVar IPCMessage -> IO ThreadId
timeoutWorker db ch = forkIO $ forever $ do
  curTime <- getCurrentTime
  prevTime <- getPrevTimeFromDb db
  saveCurTimeToDb db curTime

  print $ "... " ++ show prevTime ++ " : " ++ show curTime
                 ++ " / " ++ show (diffUTCTime curTime prevTime)
                 ++ " / " ++ show (realToFrac CFG.timeoutThreshod)

  case shouldForceUpdate prevTime curTime of
    True -> sendUpdateMessage ch
    _    -> print $ "."

  threadDelay CFG.timeoutWorkerDelay

  where
  getPrevTimeFromDb db = do
    (_, _, _, oldPrevTime) <- getPrevState db
    return oldPrevTime

  saveCurTimeToDb = writeTime

  sendUpdateMessage ch = putMVar ch MReloadFeed


streamWorker :: MyDb -> MVar [Tweet] -> IO ThreadId
streamWorker db m = forkIO $ do
  withManager$ \mgr -> do
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
    handleTL (SStatus s)          = handleTweet $ BLC.statusToTweet s
    handleTL (SRetweetedStatus s) = handleTweet $ BLC.retweetStatusToTweet s

    handleTL s = print $ "???? got not a tweet: " ++ show s

    handleTweet t = handleIncomingTweets db m [t]