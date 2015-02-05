{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import qualified BL.Core                   as BLC
import           BL.DataLayer              (MyDb, getPrevState, openDb)

import           BL.Types                  (AppState (..), IPCMessage (..),
                                            Tweet (..), makeAppState)
import           BL.Worker                 (streamWorker, timeoutWorker)
import           Config                    (port)

import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           UI.CLI.Cli                (Args (..), cliClientWorker,
                                            parseArgs)
import           UI.HTTP.App               (app)

import           Control.Concurrent        (MVar, ThreadId, forkIO, killThread,
                                            myThreadId, newEmptyMVar, newMVar, putMVar,
                                            readMVar, swapMVar, takeMVar)
import           Control.Monad             (forever)

import           Prelude                   hiding (error)
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

import qualified Control.Exception         as E
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
httpWorker app =  forkIO $ run port app

runManager :: forall b. MVar (AppState MyDb) -> IO b
runManager = monitorAppBus
    where
    monitorAppBus rs = forever $ do
        (RunState db _ _ _ fv av) <- readMVar rs

        cmd <- takeMVar av
        case cmd of
            MReloadFeed -> do
                info "Reloading feed"
                restartStreamWorker rs
                BLC.updateFeed db fv

            MNOOP ->
                info "Monitor: NOOP"

            MExit -> do
                alert "Bye bye :-)"
                exitWith $ ExitFailure 1

            _ ->
                warn "Unknown command: " -- ++ show cmd

    restartStreamWorker rs = do
        (RunState db twi maybeSwi hwi fv av) <- readMVar rs
        case maybeSwi of
            Just swi -> do
                info "Killing old stream worker... "
                killThread swi
                info "done"

                info "Starting new stream worker... "
                newWorkerId <- streamWorker db fv
                info "done"

                _ <- swapMVar rs (RunState db twi (Just newWorkerId) hwi fv av)
                return ()
            Nothing -> warn "no swi"



handleAction :: String -> MVar (AppState MyDb) -> IO ()
handleAction "serve" rs = do
    (RunState db _ _ _ fv av) <- readMVar rs

    info $ "Listening on port " ++ show port
    app_ <- app db fv
    hwid <- httpWorker app_

    info "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    info "Updating feed"
    BLC.updateFeed db fv

    info "Starting a streamWorker"
    swid <- streamWorker db fv

    _ <- swapMVar rs (RunState db (Just twid) (Just swid) (Just hwid) fv av)

    runManager rs

handleAction "cli" rs = do
    (RunState db _ _ _ fv av) <- readMVar rs

    info "Starting CLI client"
    cwid <- cliClientWorker fv

    info "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    info "Updating feed"
    BLC.updateFeed db fv

    info "Starting a streamWorker"
    swid <- streamWorker db fv

    _ <- swapMVar rs (RunState db (Just twid) (Just swid) (Just cwid) fv av)

    runManager rs

handleAction "dump" rs = do
    (RunState db _ _ _ _ _) <- readMVar rs
    (lastSeen, prevTime) <- getPrevState db

    putStrLn "Store dump:"
    putStrLn $ "last seen id: " ++ show lastSeen
             ++ "\nprev time: " ++ show prevTime

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

          runstate <- newMVar $ makeAppState db Nothing Nothing Nothing fv av

          installHandler keyboardSignal (ctrlCHandler av) Nothing

          handleAction action runstate

      Nothing -> putStrLn usage

