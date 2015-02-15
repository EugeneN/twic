{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import qualified BL.Core                   as BLC
import           BL.DataLayer              (MyDb, getPrevState, openDb)

import           BL.Types                  (AppState (..), IPCMessage (..),
                                            Tweet (..), makeAppState)
import           BL.Worker                 (streamWorker, timeoutWorker,
                                            updateWorker)
import           Config                    (port)

import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           UI.CLI.Cli                (Args (..), cliClientWorker,
                                            parseArgs)
import           UI.HTTP.App               (app)

import           Control.Concurrent        (MVar, ThreadId, forkIO, killThread,
                                            myThreadId, newEmptyMVar, newMVar,
                                            putMVar, readMVar, swapMVar,
                                            takeMVar)
import           Control.Monad             (forever)

import           Prelude                   hiding (error)
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

import qualified BL.CloudDataLayer         as CDL
import qualified Control.Exception         as E
import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           System.Exit
import           System.Posix.Signals

logRealm = "Main"

info = infoM logRealm
warn = warningM logRealm
error = errorM logRealm
alert = alertM logRealm

usage :: String
usage = "Usage: <me> serve | cli | dump"


httpWorker :: Application -> IO ThreadId
httpWorker =  forkIO . run port

runManager :: forall b. MVar (AppState MyDb) -> IO b
runManager = monitorAppBus
    where
    monitorAppBus rs = forever $ do
        (RunState st _ _ _ _ _ _ av uv) <- readMVar rs

        cmd <- takeMVar av
        case cmd of
            MReloadFeed -> do
                info "Reloading feed"
                restartStreamWorker rs
                BLC.updateFeed uv

            MNOOP ->
                info "Monitor: NOOP"

            MExit -> do
                rt <- BLC.getRunTime st
                alert $ "Run time: " ++ show rt ++ "\nBye bye :-)"
                exitWith $ ExitFailure 1

            _ ->
                warn "Unknown command: " -- ++ show cmd

    restartStreamWorker rs = do
        (RunState st db twi maybeSwi hwi uvi fv av uv) <- readMVar rs
        case maybeSwi of
            Just swi -> do
                info "Killing old stream worker... "
                killThread swi
                info "done"

                info "Starting new stream worker... "
                newWorkerId <- streamWorker db fv
                info "done"

                _ <- swapMVar rs (RunState st db twi (Just newWorkerId) hwi uvi fv av uv)
                return ()
            Nothing -> warn "no swi"



handleAction :: String -> MVar (AppState MyDb) -> IO ()
handleAction "serve" rs = do
    (RunState st db _ _ _ _ fv av uv) <- readMVar rs

    info $ "Listening on port " ++ show port
    app_ <- app st db fv uv
    hwid <- httpWorker app_

    info "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    info "Starting an updateWorker"
    uwid <- updateWorker db fv uv

    info "Starting a streamWorker"
    swid <- streamWorker db fv

    _ <- swapMVar rs (RunState st db (Just twid) (Just swid) (Just hwid) (Just uwid) fv av uv)

    runManager rs

handleAction "cli" rs = do
    (RunState st db _ _ _ _ fv av uv) <- readMVar rs

    info "Starting CLI client"
    cwid <- cliClientWorker fv

    info "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    info "Starting an updateWorker"
    uwid <- updateWorker db fv uv

    info "Updating feed"
    BLC.updateFeed uv

    info "Starting a streamWorker"
    swid <- streamWorker db fv

    _ <- swapMVar rs (RunState st db (Just twid) (Just swid) (Just cwid) (Just uwid) fv av uv)

    runManager rs

handleAction "dump" rs = do
    (RunState st db _ _ _ _ _ _ _) <- readMVar rs
    (z, prevTime, rt) <- BLC.getStatus st db

    putStrLn "Store dump:"
    putStrLn $ "last seen id: " ++ show z
             ++ "\nPrev time: " ++ show prevTime
             ++ "\nRun time: " ++  show rt

handleAction _ _ = putStrLn usage

ctrlCHandler :: MVar IPCMessage -> Handler
ctrlCHandler av = Catch $ do
    alert "Got Ctrl-C, sending exit cmd"
    putMVar av MExit

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)

  h <- fileHandler "twic.log" DEBUG >>= \lh -> return $
    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (addHandler h)

  args <- parseArgs
  case args of
      Just (Args action) -> do
          db <- openDb
          fv <- newMVar ([] :: [Tweet])
          av <- newEmptyMVar
          uv <- newEmptyMVar
          now <- getCurrentTime

          runstate <- newMVar $ makeAppState now db Nothing Nothing Nothing Nothing fv av uv

          installHandler keyboardSignal (ctrlCHandler av) Nothing

          handleAction action runstate

      Nothing -> putStrLn usage

