module BL.Worker where

import Control.Concurrent
import Control.Monad (forever)

import BL.Core       (saveFeed, getHomeFeed, readApi, homeTimelineSince, homeTimeline)
import BL.DataLayer  (MyDb, getPrevState)
import BL.Types

getFeedUrl :: TweetId -> Feed
getFeedUrl x | x == 0 = homeTimeline 0
getFeedUrl x | x > 0 = homeTimelineSince x

getMaxId :: [Tweet] -> TweetId -> TweetId
getMaxId [] oldMaxAvailableId = oldMaxAvailableId
getMaxId ts _ = BL.Types.id $ maximum ts


worker :: MyDb -> Int -> IO ThreadId
worker db delay = forkIO $ do
    forever $ do
        (_, oldMaxAvailableId, _) <- getPrevState db
        let feedUrl = getFeedUrl oldMaxAvailableId

        print $ "<<<< worker read api " ++ show feedUrl

        (feed, res) <- readApi feedUrl

        case res of
            Left err ->
              print $ show err

            Right ts -> do
              print $ ">>>> got new data: newMaxAvailableId " ++ show newMaxAvailableId ++ ", newCountNew " ++ show newCountNew
              saveFeed db newMaxAvailableId newCountNew
              where
                newMaxAvailableId = getMaxId ts oldMaxAvailableId
                newCountNew = length ts


        threadDelay delay