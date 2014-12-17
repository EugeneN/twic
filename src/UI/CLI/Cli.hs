{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module UI.CLI.Cli (
  Args(..), parseArgs
  ) where

import           System.Environment            (getArgs)
import           BL.Core                       (Username(..), Tweet(..), Author(..))
import           Control.Monad                 (mapM_)
import           Data.Text                     (unpack)

type Action = String

data Args = Args { action  :: Action
                 , count   :: Int
                 } deriving Show

showTweet :: Tweet -> String
showTweet (Tweet body created id (Author username aid screen_name hasAvatar avatarUrl) _) =
  "- " ++ (show id) ++ " " ++ (unpack username) ++ ": " ++ (show body)

parseArgs :: IO (Maybe Args)
parseArgs = do
  args <- getArgs

  case args of
    [action, countStr] | [(count,_)] <- reads countStr -> do
      return $ Just $ Args action count

    _ -> return Nothing


printTweets :: String -> [Tweet] -> IO ()
printTweets feedName ts = do
  putStrLn feedName
  putStrLn $ underline feedName
  case ts of
    [] -> putStrLn "No new tweets"
    _  -> do
      putStrLn $ (show $ length ts) ++ " new tweets"
      mapM_ (putStrLn . showTweet) ts
  
  putStrLn ""

  where 
    underline :: String -> String
    underline p = foldl (++) "" (take (length feedName) $ repeat "=")
