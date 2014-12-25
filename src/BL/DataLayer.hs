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


data Store = Store { lastSeenId     :: TweetId
                   , maxAvailableId :: TweetId
                   , countNew       :: Int
                   --, cachedFeed   :: [Tweet]
                   } deriving Show

instance SafeCopy Store where
    putCopy Store{..} = contain $ do safePut lastSeenId; safePut maxAvailableId; safePut countNew
    getCopy = contain $ Store <$> safeGet <*> safeGet <*> safeGet

data Database = Database Store deriving Show

$(deriveSafeCopy 0 'base ''Database)

updateStore :: TweetId -> Int -> Update Database ()
updateStore newMaxAvailableId newCountNew = do
  Database (Store oldLastSeen oldMaxAvailableId oldCountNew) <- get
  put $ Database (Store oldLastSeen newMaxAvailableId (newCountNew + oldCountNew))

viewStore :: Int -> Update Database (TweetId, Int)
viewStore limit = do
  Database (Store oldLastSeen oldMaxAvailableId oldCountNew) <- get
  put $ Database (Store oldMaxAvailableId oldMaxAvailableId 0)

  return (oldLastSeen, oldCountNew)

justView :: Query Database (TweetId, TweetId, Int)
justView = do
  Database (Store oldLastSeen oldMaxAvailableId oldCountNew) <- ask
  return (oldLastSeen, oldMaxAvailableId, oldCountNew)

$(makeAcidic ''Database ['updateStore, 'viewStore, 'justView])

type MyDb = AcidState Database

openDb :: IO MyDb
openDb = openLocalStateFrom "myDatabase/" (Database $ Store 0 0 0)

write :: MyDb -> TweetId -> Int -> IO (EventResult UpdateStore)
write db tid count = update db (UpdateStore tid count)

read :: MyDb -> Int -> IO (TweetId, Int)
read db limit = update db (ViewStore limit)

getPrevState :: MyDb -> IO (TweetId, TweetId, Int)
getPrevState db = query db JustView