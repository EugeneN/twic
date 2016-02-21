{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified BL.Core                   as BLC
import           BL.DataLayer              (MyDb, getPrevState, openDb)

import           BL.Types
import           BL.Worker                 (accountFetchWorker, streamWorker,
                                            timeoutWorker, updateWorker)
import           Config                    (port, userConfig, logFile)

import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           UI.CLI.Cli                (Args (..), cliClientWorker,
                                            parseArgs)
import           UI.HTTP.App               (app)

import           Control.Concurrent        (MVar, ThreadId, forkIO, killThread,
                                            myThreadId, newEmptyMVar, newMVar,
                                            putMVar, readMVar, swapMVar,
                                            takeMVar)
import           Control.Monad             (forever, void)

import           Prelude                   hiding (error)
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

import qualified BL.CloudDataLayer         as CDL
import qualified Control.Exception         as E
import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           System.Exit

import           Data.Configurator
import           Data.Configurator.Types
import           Data.Text                 (pack, unpack)

-- # if defined(mingw32_HOST_OS)
--import GHC.IO.IOMode
-- import System.Win32.DebugApi (PHANDLE)
-- # else
import System.Posix.Signals
-- # endif

import           System.Posix.Signals
import           System.Process

logRealm = "Main"

info = infoM logRealm
warn = warningM logRealm
error = errorM logRealm
alert = alertM logRealm

usage :: String
usage = "Usage: <me> serve | cli | dump"

confUsage :: String
confUsage = "\nExpecting `" ++ userConfig ++ "` config file with Twitter OAuth settings.\n"
         ++ "See https://github.com/EugeneN/twic/wiki/Quick-start#how-to-configure-and-run-twic\n"

logUsage :: String
logUsage = "\nExpecting `" ++ logFile ++ "` log file.\n"
        ++ "See https://github.com/EugeneN/twic/wiki/Quick-start#how-to-configure-and-run-twic\n"

badConf :: String
badConf = "\nBad or uncomplete `" ++ userConfig ++ "` config file.\n"
       ++ "See https://github.com/EugeneN/twic/wiki/Quick-start#how-to-configure-and-run-twic\n"


httpWorker :: Application -> IO ThreadId
httpWorker =  forkIO . run port

runManager :: forall b. MVar (AppState MyDb) -> IO b
runManager = monitorAppBus
    where
    monitorAppBus rs = forever $ do
        (RunState st _ _ _ _ _ _ _ av uv _ _) <- readMVar rs

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
                -- TODO implement restart
                exitWith $ ExitFailure 2

            MRestart -> do
                rt <- BLC.getRunTime st
                alert $ "Run time: " ++ show rt ++ "\nSee you in a moment:-)"
                exitWith $ ExitFailure 1


            _ ->
                warn "Unknown command: " -- ++ show cmd

    restartStreamWorker rs = do
        (RunState st db twi maybeSwi hwi uvi afwi fv av uv accv cfg) <- readMVar rs
        case maybeSwi of
            Just swi -> do
                info "Killing old stream worker... "
                killThread swi
                info "done"

                info "Starting new stream worker... "
                newWorkerId <- streamWorker db fv cfg
                info "done"

                _ <- swapMVar rs (RunState st db twi (Just newWorkerId) hwi uvi afwi fv av uv accv cfg)
                return ()
            Nothing -> warn "no swi"



handleAction :: String -> MVar (AppState MyDb) -> IO ()
handleAction "serve" rs = do
    (RunState st db _ _ _ _ _ fv av uv accv cfg) <- readMVar rs

    info $ "Listening on port " ++ show port
    app_ <- app st db fv uv accv cfg
    hwid <- httpWorker app_

    info "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    info "Starting an updateWorker"
    uwid <- updateWorker db fv uv cfg

    info "Starting a streamWorker"
    swid <- streamWorker db fv cfg

    info "Starting an account fetch worker"
    acwid <- accountFetchWorker accv fv cfg

    _ <- swapMVar rs (RunState st db (Just twid) (Just swid) (Just hwid) (Just uwid) (Just acwid) fv av uv accv cfg)

    rc <- system $ "open http://localhost:" ++ show port

    runManager rs

handleAction "cli" rs = do
    (RunState st db _ _ _ _ _ fv av uv accv cfg) <- readMVar rs

    info "Starting CLI client"
    cwid <- cliClientWorker fv

    info "Starting a timeoutWorker"
    twid <- timeoutWorker db av

    info "Starting an updateWorker"
    uwid <- updateWorker db fv uv cfg

    info "Updating feed"
    BLC.updateFeed uv

    info "Starting a streamWorker"
    swid <- streamWorker db fv cfg

    info "Starting an account fetch worker"
    acwid <- accountFetchWorker accv fv cfg

    _ <- swapMVar rs (RunState st db (Just twid) (Just swid) (Just cwid) (Just uwid) (Just acwid) fv av uv accv cfg)

    runManager rs

handleAction "dump" rs = do
    (RunState st db _ _ _ _ _ _ _ _ _ cfg) <- readMVar rs
    (z, prevTime, rt) <- BLC.getStatus st db cfg

    putStrLn $ "Cfg dump:"
    putStrLn $ show cfg

    putStrLn ""
    putStrLn "Store dump:"
    putStrLn $ "last seen id: " ++ show z
             ++ "\nPrev time: " ++ show prevTime
             ++ "\nRun time: " ++  show rt

handleAction _ _ = putStrLn usage

ctrlCHandler :: MVar IPCMessage -> Handler
ctrlCHandler av = Catch $ do
    alert "Got Ctrl-C, sending exit cmd"
    putMVar av MExit

withConfig :: FilePath -> (Cfg -> IO ()) -> IO ()
withConfig name t = do
    mb <- E.try $ load [Required name]
    case mb of
        Left (err :: E.SomeException) -> putStrLn ("Error: " ++ show err) >> putStrLn confUsage

        Right cfg -> do
          ck  <- lkp cfg "oauthConsumerKey"
          cks <- lkp cfg "oauthConsumerSecret"
          at  <- lkp cfg "accessToken"
          ats <- lkp cfg "accessTokenSecret"
          cdb <- lkp cfg "cloudDbUrl"

          case Cfg <$> ck <*> cks <*> at <*> ats <*> cdb of
            Nothing -> putStrLn badConf
            Just c  -> t c

          where
            lkp cfg i = do
              v <- Data.Configurator.lookup cfg i :: IO (Maybe String)
              return v

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)

  h <- E.try $ fileHandler logFile DEBUG >>= \lh -> return $
    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

  case h of
    Left (err :: E.SomeException) -> putStrLn ("Error: " ++ show err) >> putStrLn logUsage
    Right h'                      -> updateGlobalLogger rootLoggerName (addHandler h')

  args <- parseArgs
  case args of
      Just (Args action) -> withConfig userConfig $ \cfg -> do
          db       <- openDb
          fv       <- newMVar ([] :: FeedState)
          av       <- newEmptyMVar
          uv       <- newEmptyMVar
          accv     <- newEmptyMVar
          now      <- getCurrentTime

          runstate <- newMVar $ makeAppState now db Nothing Nothing Nothing Nothing Nothing fv av uv accv cfg

          installHandler keyboardSignal (ctrlCHandler av) Nothing

          handleAction action runstate

      Nothing -> putStrLn usage
