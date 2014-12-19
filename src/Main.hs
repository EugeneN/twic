{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           UI.CLI.Cli               (parseArgs, Args(..))
import           UI.HTTP.App              (app)
import           BL.DataLayer             (openDb, getPrevState)
import           BL.Worker                (worker)
import           Config                   (port, delay)

usage = "Usage: <me> serve|dump tweets-count"

--handleAction :: Action -> myDb -> IO
handleAction "serve" db count = do
    putStrLn $ "Starting a worker with delay " ++ show delay
    workerId <- worker db delay

    putStrLn $ "Listening on port " ++ show port
    run port (app db count)

handleAction "dump" db _ = do
    putStrLn $ "Store dump:"

    (lastSeen, maxAvailableId, countNew) <- getPrevState db

    putStrLn $ "last seen id: " ++ show lastSeen
             ++ "\nmax available id: " ++ show maxAvailableId
             ++ "\ncount new: " ++ show countNew

handleAction _ _ _ = putStrLn usage


main :: IO ()
main = do
  args <- parseArgs

  case args of
      Just (Args action count) -> do
          db <- openDb
          handleAction action db count

      Nothing -> putStrLn usage

