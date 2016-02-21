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
import           Prelude                   hiding (error, lookup)
import           System.Log.Handler.Simple
import           System.Log.Logger
import qualified Web.Twitter.Types         as TT

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

accountFetchWorker :: MVar UpdateMessage -> MVar FeedState -> Cfg -> IO ThreadId
accountFetchWorker accv fv cfg = forkIO $ forever $ do
    fetchreq <- takeMVar accv
    debug $ "Got fetch account request at " ++ show fetchreq
    BLC.fetchContext fv cfg

updateWorker :: MyDb -> MVar FeedState -> MVar UpdateMessage -> Cfg -> IO ThreadId
updateWorker db fv uv cfg = forkIO $ forever $ do
    ur <- takeMVar uv
    debug $ "***Got an update request at " ++ show ur
    -- TODO throttle update requests here
    BLC.updateFeedSync db fv cfg

streamWorker :: MyDb -> MVar FeedState -> Cfg -> IO ThreadId
streamWorker db m cfg = forkIO $
  withManager$ \mgr -> do
    src <- stream (BLC.twInfo cfg) mgr userstream
    src C.$$+- CL.mapM_ (^! act (liftIO . handleTL))

  where
  handleTL :: StreamingAPI -> IO ()
  handleTL (SStatus s)          = handleTweet $ BLC.statusToTweet s
  handleTL (SRetweetedStatus s) = handleTweet $ BLC.retweetStatusToTweet s
  handleTL (SEvent ev)          = handleEvent ev
  handleTL s = debug $ "???? got not a tweet: " ++ show s

  handleTweet t = BLC.handleIncomingFeedMessages db m [TweetMessage t]

  handleEvent ( TT.Event { evCreatedAt = _
                         , evTargetObject = _
                         , evEvent = "follow"
                         , evTarget = (ETUser user)
                         , evSource = _}) = BLC.handleIncomingFeedMessages db m [UserMessage user]

  handleEvent x = debug $ "???? got unknown event: " ++ show x
