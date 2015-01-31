{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module UI.CLI.Cli (
  Args(..), parseArgs, cliClientWorker
  ) where

import           BL.Core            (Author (..), Tweet (..), Username)
import           Control.Monad      (forever, mapM_)
import           Data.Text          (unpack)
import           System.Environment (getArgs)

import           Control.Concurrent (MVar, ThreadId, forkIO, killThread,
                                     newEmptyMVar, newMVar, putMVar, readMVar,
                                     swapMVar, takeMVar, tryPutMVar)
import           Data.Functor       ((<$>))

type Action = String

data Args = Args { action :: Action
                 } deriving Show

showTweet :: Tweet -> String
showTweet (Tweet body created id id_str (Author username aid screen_name hasAvatar avatarUrl) _ _) =
  "- " ++ show id ++ " " ++ unpack username ++ ": " ++ show body

parseArgs :: IO (Maybe Args)
parseArgs = do
  args <- getArgs

  case args of
    [action] ->
      return $ Just $ Args action

    _ -> return Nothing


cliClientWorker :: MVar [Tweet] -> IO ThreadId
cliClientWorker fv = forkIO $ forever $
    takeMVar fv >>= (print . show)

printTweets :: String -> [Tweet] -> IO ()
printTweets feedName ts = do
  putStrLn feedName
  putStrLn $ underline feedName
  case ts of
    [] -> putStrLn "No new tweets"
    _  -> do
      putStrLn $ show (length ts) ++ " new tweets"
      mapM_ (putStrLn . showTweet) ts

  putStrLn ""

  where
    underline :: String -> String
    underline p = foldl (++) "" (replicate (length feedName) "=")
