{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module BL.DataLayer where

import           Data.Acid
import           Data.Acid.Advanced

import           BL.Types
import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.SafeCopy
import           Data.Time.Clock      (UTCTime (..), getCurrentTime)
import           Prelude              hiding (id)


data Store = Store { lastSeenId :: TweetId
                   , prevTime   :: UTCTime
                   } deriving Show

data Database = Database Store deriving Show

type MyDb = AcidState Database

instance SafeCopy Store where
    putCopy Store{..} = contain $ do
        safePut lastSeenId
        safePut prevTime
    getCopy = contain $ Store <$> safeGet <*> safeGet

$(deriveSafeCopy 0 'base ''Database)

-------------------------------------------------------------------------------

updateLastSeen :: TweetId -> Update Database ()
updateLastSeen newLastSeen = do
  Database (Store _ oldPrevTime) <- get
  put $ Database (Store newLastSeen oldPrevTime)

updateTime :: UTCTime -> Update Database ()
updateTime time = do
  Database (Store oldLastSeen _) <- get
  put $ Database (Store oldLastSeen time)

justView :: Query Database (TweetId, UTCTime)
justView = do
  Database (Store lastSeen prevTime) <- ask
  return (lastSeen, prevTime)

$(makeAcidic ''Database ['updateLastSeen, 'updateTime, 'justView])

-------------------------------------------------------------------------------

openDb :: IO MyDb
openDb = do
    curTime <- getCurrentTime
    openLocalStateFrom "twic.db/" (Database $ Store 0 curTime)

getPrevState :: MyDb -> IO (TweetId, UTCTime)
getPrevState db = query db JustView

writeLastSeen :: MyDb -> TweetId -> IO (EventResult UpdateLastSeen)
writeLastSeen db tid = update db (UpdateLastSeen tid)

writeTime :: MyDb -> UTCTime -> IO (EventResult UpdateTime)
writeTime db time = update db (UpdateTime time)
