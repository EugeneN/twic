{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}

module BL.Worker where

import           Control.Concurrent

import           Web.Twitter.Conduit
import           Web.Twitter.Types.Lens

import           Control.Applicative
import           Control.Lens

import           Control.Lens.Action       (act, (^!))

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8     as BS
import           Data.Conduit
import qualified Data.Conduit              as C
import qualified Data.Conduit.Binary       as CB
import qualified Data.Conduit.List         as CL
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Data.Time.Clock           (UTCTime (..), diffUTCTime,
                                            getCurrentTime, secondsToDiffTime)
import           Network.HTTP.Conduit      as HTTP
import           Web.Authenticate.OAuth

import qualified BL.Core                   as BLC
import           BL.DataLayer              (MyDb, getPrevState, writeTime)
import           BL.Types
import qualified Config                    as CFG
import           Prelude                   hiding (error)
import           System.Log.Handler.Simple
import           System.Log.Logger

logRealm = "Worker"

info = infoM logRealm
warn = warningM logRealm
debug = debugM logRealm
error = errorM logRealm

shouldForceUpdate :: UTCTime -> UTCTime -> Bool
shouldForceUpdate prev cur =
  diffUTCTime cur prev > realToFrac CFG.timeoutThreshod

-- TODO handle http timeouts, not just sleep mode
timeoutWorker :: MyDb -> MVar IPCMessage -> IO ThreadId
timeoutWorker db ch = forkIO $ forever $ do
  curTime <- getCurrentTime
  prevTime <- getPrevTimeFromDb db
  saveCurTimeToDb db curTime

  debug $ "... " ++ show prevTime ++ " : " ++ show curTime
                 ++ " / " ++ show (diffUTCTime curTime prevTime)
                 ++ " / " ++ show (realToFrac CFG.timeoutThreshod)

  if shouldForceUpdate prevTime curTime
    then sendUpdateMessage ch
    else debug "."

  threadDelay CFG.timeoutWorkerDelay

  where
  getPrevTimeFromDb db = do
    (_, oldPrevTime) <- getPrevState db
    return oldPrevTime

  saveCurTimeToDb = writeTime

  sendUpdateMessage ch = putMVar ch MReloadFeed

updateWorker :: MyDb -> MVar [Tweet] -> MVar UpdateMessage -> IO ThreadId
updateWorker db fv uv = forkIO $ forever $ do
    ur <- takeMVar uv
    debug $ "***Got an update request at " ++ show ur
    -- TODO throttle update requests here
    BLC.updateFeedSync db fv

streamWorker :: MyDb -> MVar [Tweet] -> IO ThreadId
streamWorker db m = forkIO $
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

    handleTL s = debug $ "???? got not a tweet: " ++ show s

    handleTweet t = BLC.handleIncomingTweets db m [t]
