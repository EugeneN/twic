{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import qualified BL.Core                  as BLC
import           BL.DataLayer             (MyDb, getPrevState, openDb)

import           BL.Types                 (IPCMessage (..), Tweet (..))
import           BL.Worker                (handleIncomingTweets, streamWorker,
                                           timeoutWorker)
import           Config                   (port)

import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           UI.CLI.Cli               (Args (..), cliClientWorker,
                                           parseArgs)
import           UI.HTTP.App              (app)

import           Control.Concurrent       (MVar, ThreadId, forkIO, killThread,
                                           newEmptyMVar, newMVar, putMVar,
                                           readMVar, swapMVar, takeMVar,
                                           tryPutMVar)
import           Control.Monad            (forever)

usage = "Usage: <me> serve | cli | dump tweets-count"

data AppState = RunState { db              :: MyDb
                         , timeoutWorkerId :: Maybe ThreadId
                         , streamWorkerId  :: Maybe ThreadId
                         , uiWorkerId      :: Maybe ThreadId
                         , feedVar         :: MVar [Tweet]
                         , appBusVar       :: MVar IPCMessage
                         } -- deriving Show

httpWorker :: Application -> IO ThreadId
httpWorker app =  forkIO $ run port app

runManager :: forall b. MVar AppState -> IO b
runManager = monitorAppBus
    where
    monitorAppBus rs = forever $ do
        (RunState _ _ _ _ _ av) <- readMVar rs

        cmd <- takeMVar av
        case cmd of
            MReloadFeed -> do
                restartStreamWorker rs
                updateFeed rs

            MNOOP ->
                print "Monitor: NOOP"

            _ ->
                print "Unknown command: " -- ++ show cmd

    restartStreamWorker rs = do
        (RunState db twi maybeSwi hwi fv av) <- readMVar rs
        case maybeSwi of
            Just swi -> do
                putStr "Killing old stream worker... "
                killThread swi
                putStrLn "done"

                putStr "Starting new stream worker... "
                newWorkerId <- streamWorker db fv
                putStrLn "done"

                swapMVar rs (RunState db twi (Just newWorkerId) hwi fv av)
                return ()
            Nothing -> print "no swi"


    updateFeed rs = do
        (RunState db _ _ _ fv _) <- readMVar rs
        feedUrl <- BLC.getCachedFeed db 0
        (feed, res) <- BLC.readApi feedUrl

        case res of
            Right ts -> handleIncomingTweets db fv (reverse ts)
            Left err -> print $ "error" ++ show err

handleAction :: String -> MVar AppState -> Int -> IO ()
handleAction "serve" rs count = do
    (RunState db _ _ _ fv av) <- readMVar rs

    putStrLn "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    putStrLn "Starting a streamWorker"
    swid <- streamWorker db fv

    putStrLn $ "Listening on port " ++ show port
    app_ <- app db fv count
    hwid <- httpWorker app_

    swapMVar rs (RunState db (Just twid) (Just swid) (Just hwid) fv av)

    runManager rs

handleAction "cli" rs count = do
    (RunState db _ _ _ fv av) <- readMVar rs

    putStrLn "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    putStrLn "Starting a streamWorker"
    swid <- streamWorker db fv

    putStrLn "Starting CLI client"
    cwid <- cliClientWorker fv

    _ <- swapMVar rs (RunState db (Just twid) (Just swid) (Just cwid) fv av)

    runManager rs

handleAction "dump" rs _ = do
    (RunState db _ _ _ _ _) <- readMVar rs
    (lastSeen, maxAvailableId, countNew, prevTime) <- getPrevState db

    putStrLn "Store dump:"
    putStrLn $ "last seen id: " ++ show lastSeen
             ++ "\nmax available id: " ++ show maxAvailableId
             ++ "\ncount new: " ++ show countNew
             ++ "\nprev time: " ++ show prevTime

handleAction _ _ _ = putStrLn usage


main :: IO ()
main = do
  args <- parseArgs

  case args of
      Just (Args action count) -> do
          db <- openDb
          fv <- newMVar ([] :: [Tweet])
          av <- newEmptyMVar

          runstate <- newMVar (RunState db Nothing Nothing Nothing fv av)

          handleAction action runstate count

      Nothing -> putStrLn usage

