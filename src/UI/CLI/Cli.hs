{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module UI.CLI.Cli (
  Args(..), parseArgs, cliClientWorker
  ) where

import           BL.Core            (Author (..), Tweet (..), Username)
import           BL.Types
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

parseArgs :: IO (Maybe Args)
parseArgs = do
  args <- getArgs

  case args of
    [action] ->
      return $ Just $ Args action

    _ -> return Nothing


cliClientWorker :: MVar FeedState -> IO ThreadId
cliClientWorker fv = forkIO $ forever $
    takeMVar fv >>= (print . show)

