{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module BL.DataLayer where

import           Data.Acid
import           Data.Acid.Advanced

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.SafeCopy
import           BL.Types
import           Prelude              hiding (id)
import Data.Time.Clock (diffUTCTime, getCurrentTime, UTCTime(..))


data Store = Store { lastSeenId     :: TweetId
                   , maxAvailableId :: TweetId
                   , countNew       :: Int
                   , prevTime       :: UTCTime
                   --, cachedFeed   :: [Tweet]
                   } deriving Show

instance SafeCopy Store where
    putCopy Store{..} = contain $ do
        safePut lastSeenId
        safePut maxAvailableId
        safePut countNew
        safePut prevTime
    getCopy = contain $ Store <$> safeGet <*> safeGet <*> safeGet <*> safeGet

data Database = Database Store deriving Show

$(deriveSafeCopy 0 'base ''Database)

updateStore :: TweetId -> Int -> Update Database ()
updateStore newMaxAvailableId newCountNew = do
  Database (Store oldLastSeen oldMaxAvailableId oldCountNew oldPrevTime) <- get
  put $ Database (Store oldLastSeen newMaxAvailableId (newCountNew + oldCountNew) oldPrevTime)

updateTime :: UTCTime -> Update Database ()
updateTime time = do
  Database (Store oldLastSeen oldMaxAvailableId oldCountNew _) <- get
  put $ Database (Store oldLastSeen oldMaxAvailableId oldCountNew time)

viewStore :: Int -> Update Database (TweetId, Int)
viewStore limit = do
  Database (Store oldLastSeen oldMaxAvailableId oldCountNew oldPrevTime) <- get
  put $ Database (Store oldMaxAvailableId oldMaxAvailableId 0 oldPrevTime)

  return (oldLastSeen, oldCountNew)

justView :: Query Database (TweetId, TweetId, Int, UTCTime)
justView = do
  Database (Store oldLastSeen oldMaxAvailableId oldCountNew oldPrevTime) <- ask
  return (oldLastSeen, oldMaxAvailableId, oldCountNew, oldPrevTime)

$(makeAcidic ''Database ['updateStore, 'viewStore, 'justView, 'updateTime])

type MyDb = AcidState Database

openDb :: IO MyDb
openDb = do
    curTime <- getCurrentTime
    openLocalStateFrom "myDatabase/" (Database $ Store 0 0 0 curTime)

write :: MyDb -> TweetId -> Int -> IO (EventResult UpdateStore)
write db tid count = update db (UpdateStore tid count)

read :: MyDb -> Int -> IO (TweetId, Int)
read db limit = update db (ViewStore limit)

getPrevState :: MyDb -> IO (TweetId, TweetId, Int, UTCTime)
getPrevState db = query db JustView

writeTime :: MyDb -> UTCTime -> IO (EventResult UpdateTime)
writeTime db time = update db (UpdateTime time)
