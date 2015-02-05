module Config where

serverName = "api.twitter.com"
oauthConsumerKey = "key"
oauthConsumerSecret = "key-secret"

accessToken = "token"
accessTokenSecret = "token-secret"

oneSecond = 1000000 :: Int
oneMinute = 60 * oneSecond

port = 3000 :: Int
delay = 2 * oneMinute

heartbeatDelay = 15 :: Int -- seconds

timeoutThreshod = 90
timeoutWorkerDelay = (timeoutThreshod * oneSecond) - oneSecond

updateRetryCount = 3 :: Int
updateRetryDelay = 2 * oneSecond
updateFeedAsync = False

cloudDbUrl = "https://***.firebaseio.com/.json" :: String