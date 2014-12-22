module Config where

serverName = "api.twitter.com"
oauthConsumerKey = "s3zKH2jX6P0lzThMH95P0trPE"
oauthConsumerSecret = "HdE8AY6b3azzDtPZ5bbdyxmVX649SYKNDITlHEkCxwdAO6crn5"

accessToken = "897203563-pZzjKrxiARmML9RVw0yxsrayTeyRELpEKNWiiJDi"
accessTokenSecret = "9wEOqgMR8TJBdsx4NQBbiGZNnu3Pn8Z0jxlW2KbzMgLnJ"

oneSecond = 1000000 :: Int
oneMinute = 60 * oneSecond

port = 3000 :: Int
delay = 2 * oneMinute

heartbeatDelay = 15 :: Int -- seconds
