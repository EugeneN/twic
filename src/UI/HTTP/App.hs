{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.App where

import           Control.Monad.IO.Class
import           Network.Wai                   (responseStream, Application, pathInfo, responseLBS, responseFile, queryString)
import           Network.HTTP.Types            (status200, HeaderName)
import           Network.HTTP.Types.Header     (ResponseHeaders)
import           Blaze.ByteString.Builder      (Builder, fromByteString)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Data.ByteString
import           Data.ByteString.Char8         (readInteger)
import           Data.Int                      (Int64)

import           UI.HTTP.Json                  (justTweetsToJson, justUnreadCountToJson)
import           UI.HTTP.Html                  (tweetsToHtml, retweetToHtml, justTweetsToHtml, homePage)
import           BL.Core                       (getCachedFeed, readApi, writeApi, retweetUrl, getUnreadCount)
import           BL.Types                      (TweetId)
import           BL.DataLayer                  (MyDb)


type Filename = String

mimeHtml = ("Content-Type", "text/html")
mimeText = ("Content-Type", "text/plain")
mimeJs   = ("Content-Type", "text/javascript")
mimeJSON = ("Content-Type", "application/json")
mimeIco  = ("Content-Type", "image/x-icon")

app :: MyDb -> Int -> Application -- = Request -> ResourceT IO Response
app db count request sendResponse = do
  print $ pathInfo request
  case pathInfo request of
    []                  -> homeHandler count request sendResponse

    ["cs", "Main.js"]   -> staticHandler [mimeJs] "dist/cs/Main.js" request sendResponse
    ["favicon.ico"]     -> staticHandler [mimeIco] "res/favicon.ico" request sendResponse

    ["update"]          -> updateHandler db count request sendResponse
    ["update", _]       -> updateHandler db count request sendResponse

    ["check"]           -> checkHandler db count request sendResponse
    ["check", _]        -> checkHandler db count request sendResponse

    ["retweet"]         -> retweetHandler count request sendResponse
    ["retweet", _]      -> retweetHandler count request sendResponse

    path                -> notFoundHandler count request sendResponse


homeHandler :: Int -> Application
homeHandler count request response = response $ responseStream status200 [mimeHtml] (homeStream count)
    where
        homeStream :: Int -> (Builder -> IO ()) -> IO () -> IO ()
        homeStream count send flush = do
          send $ renderHtmlBuilder $ homePage
          flush

staticHandler :: ResponseHeaders -> FilePath -> Application
staticHandler mime fn request response = response $ responseFile status200 mime fn Nothing

notFoundHandler :: Int -> Application
notFoundHandler count request response = response $ responseLBS status200 [mimeText] "Unknown path"

checkHandler :: MyDb -> Int -> Application
checkHandler db count request response = response $ responseStream status200 [mimeJSON] (justCheckStreamJson db count)
     where
         justCheckStreamJson :: MyDb -> Int -> (Builder -> IO ()) -> IO () -> IO ()
         justCheckStreamJson db count send flush = do
             unreadCount <- getUnreadCount db
             print $ "**** check unread count " ++ (show unreadCount)

             send $ justUnreadCountToJson unreadCount
             flush

updateHandler :: MyDb -> Int -> Application
updateHandler db count request response = response $ responseStream status200 [mimeJSON] (justFeedStreamJson db count)
    where
        justFeedStreamJson :: MyDb -> Int -> (Builder -> IO ()) -> IO () -> IO ()
        justFeedStreamJson db count send flush = do
          feedUrl <- getCachedFeed db count

          print $ "**** read api " ++ (show feedUrl)
          (feed, res) <- readApi feedUrl

          case res of
            Left err ->
              send $ justTweetsToJson $ Left err

            Right ts -> do
              send $ justTweetsToJson $ Right ts

          flush

retweetHandler :: Int -> Application
retweetHandler count request response = case queryString request of
    [("id", Just id_)] -> do
        case readInteger id_ of
          Just (int, str) -> do
            print $ "got retweet " ++ show id_
            response $ responseStream status200 [mimeHtml] (retweetStream (fromIntegral int :: Int64))

          Nothing -> do
            print ("bad retweet id" :: String)
            response $ responseLBS status200 [mimeText] "bad retweet id"

    _ -> do
        response $ responseLBS status200 [mimeText] "bad request"

    where
        retweetStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        retweetStream id_ send flush = do
            let url = retweetUrl id_
            print $ ">>>>" ++ url ++ "<<<<"
            res <- writeApi $ url
            send $ renderHtmlBuilder $ retweetToHtml res
            flush







