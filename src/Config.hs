module Config where

serverName         = "api.twitter.com"

userConfig         = "twic.cfg"
logFile            = "twic.log"

oneSecond          = 1000000 :: Int
oneMinute          = 60 * oneSecond

port               = 3000 :: Int
delay              = 2 * oneMinute

heartbeatDelay     = 15 :: Int -- seconds

timeoutThreshod    = 90
timeoutWorkerDelay = (timeoutThreshod * oneSecond) - oneSecond

updateRetryCount   = 30 :: Int
updateRetryDelay   = 2 * oneSecond
updateFeedAsync    = False
