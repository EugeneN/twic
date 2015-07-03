module Config where

serverName = "api.twitter.com"
oauthConsumerKey = "C2dfbPN6OmE9fsGHXbEEkOYmf"
oauthConsumerSecret = "uYy749NZY0lCzQro8mH1joeYglnzjGzfZEWVd8jVArsKTxnHZa"

accessToken = "897203563-rFrcObucJd4KByiOH9YSfoSYZFvwZ3DepAX6I0lF"
accessTokenSecret = "xe8EWIm79jw6uSBKWbKQWUGHukb8PHzaxCcg2OOrNxwuY"

oneSecond = 1000000 :: Int
oneMinute = 60 * oneSecond

port = 3000 :: Int
delay = 2 * oneMinute

heartbeatDelay = 15 :: Int -- seconds

timeoutThreshod = 90
timeoutWorkerDelay = (timeoutThreshod * oneSecond) - oneSecond

updateRetryCount = 30 :: Int
updateRetryDelay = 2 * oneSecond
updateFeedAsync = False

cloudDbUrl = "https://sweltering-heat-9224.firebaseio.com/.json" :: String
