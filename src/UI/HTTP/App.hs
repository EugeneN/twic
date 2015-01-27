{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.App where

import           BL.Core                        (getCachedFeed, getUnreadCount,
                                                 readApi, retweetUrl, starUrl,
                                                 tweetUrl, writeApi)
import           BL.DataLayer                   (MyDb)
import           BL.Types                       (Message (..), Tweet, TweetBody,
                                                 TweetId)
import           Blaze.ByteString.Builder       (Builder, fromByteString)
import           Config                         (heartbeatDelay)
import           Control.Applicative            ((<$>))
import           Control.Concurrent             (MVar, ThreadId, forkIO,
                                                 killThread, modifyMVar,
                                                 modifyMVar_, myThreadId,
                                                 newMVar, readMVar, takeMVar,
                                                 threadDelay)
import           Control.Exception              (fromException, handle)
import           Control.Monad                  (forM_, forever)
import           Control.Monad.IO.Class
import           Data.Aeson                     (encode)
import           Data.ByteString
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BSL
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Tuple
import           Data.UUID                      (UUID)
import           Data.UUID.V4                   (nextRandom)
import           Network.HTTP.Types             (HeaderName, status200)
import           Network.HTTP.Types.Header      (ResponseHeaders)
import           Network.Wai                    (Application, pathInfo,
                                                 queryString, responseFile,
                                                 responseLBS, responseStream)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets             as WS
import           Text.Blaze.Html.Renderer.Utf8  (renderHtmlBuilder)
import           UI.HTTP.Html                   (homePage, justTweetsToHtml,
                                                 retweetToHtml, tweetsToHtml)
import           UI.HTTP.Json                   (justTweetsToJson,
                                                 justUnreadCountToJson,
                                                 retweetToJson, starToJson,
                                                 tweetToJson)

--import qualified Network.Wai.Application.Static as Static
--import Data.FileEmbed (embedDir)

type Filename  = String
type Client    = (UUID, WS.Connection)
type WSState   = [Client]
type FeedState = [Tweet]

mimeHtml :: (HeaderName, ByteString)
mimeHtml = ("Content-Type", "text/html")
mimeText = ("Content-Type", "text/plain")
mimeJs   = ("Content-Type", "text/javascript")
mimeJSON = ("Content-Type", "application/json")
mimeIco  = ("Content-Type", "image/x-icon")

httpapp :: MyDb -> Int -> Application -- = Request -> ResourceT IO Response
httpapp db count request sendResponse = do
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

    ["star"]            -> starHandler count request sendResponse
    ["star", _]         -> starHandler count request sendResponse

    ["tweet"]           -> tweetHandler count request sendResponse
    ["tweet", _]        -> tweetHandler count request sendResponse

    path                -> notFoundHandler count request sendResponse

makeClient :: UUID -> WS.Connection -> Client
makeClient a c = (a, c)

addClient :: Client -> WSState -> WSState
addClient client clients = client : clients

removeClient :: Client -> WSState -> WSState
removeClient client = Prelude.filter ((/= fst client) . fst)

broadcast :: BSL.ByteString -> WSState -> IO ()
broadcast msg clients = forM_ clients $ \(_, conn) -> WS.sendTextData conn msg

app :: MyDb -> MVar FeedState -> Int -> IO Application
app db m count = do
    cs <- newMVar ([] :: WSState)
    _ <- startBroadcastWorker m cs
    return $ WaiWS.websocketsOr WS.defaultConnectionOptions
                                (wsapp m cs)
                                (httpapp db count)

wsapp :: MVar FeedState -> MVar WSState -> WS.ServerApp
wsapp _ cs pending = do
  conn <- WS.acceptRequest pending
  clientId <- nextRandom
  let client = makeClient clientId conn

  print $ "<<~~ Incoming ws connection " ++ show clientId

  WS.forkPingThread conn heartbeatDelay
  modifyMVar_ cs $ \cur -> return $ addClient client cur
  trackConnection client cs

startBroadcastWorker :: MVar FeedState -> MVar WSState -> IO ThreadId
startBroadcastWorker m cs = forkIO $
    forever $ do
        ts <- takeMVar m
        clients <- readMVar cs
        broadcast (encode ts) clients

trackConnection :: Client -> MVar WSState -> IO ()
trackConnection client@(clientId, clientConn) cs = handle catchDisconnect $
  forever $ do
    x <- WS.receive clientConn
    print $ "?>?> got smth from ws client " ++ show clientId ++ ": " ++ show x

    where
      catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> do
          print $ "ws ConnectionClosed for " ++ show clientId
          filterOutClient client cs
          return ()

        Just (WS.CloseRequest a b) -> do
          print $ "ws CloseRequest from client " ++ show clientId ++ ": " ++ show a ++ "/" ++ show b
          filterOutClient client cs
          return ()

        Just (WS.ParseException s) -> do
          print $ "ws ParseException for " ++ show clientId ++ ": " ++ s
          filterOutClient client cs
          return ()

        x -> do
          print $ "ws closed for unknown reason for " ++ show clientId ++ ": " ++ show x
          filterOutClient client cs
          return ()

      filterOutClient client cs =
        modifyMVar_ cs $ \clients ->
            return $ removeClient client clients

homeHandler :: Int -> Application
homeHandler count request response = response $ responseStream status200 [mimeHtml] (homeStream count)
    where
        homeStream :: Int -> (Builder -> IO ()) -> IO () -> IO ()
        homeStream count send flush = return homePage >>= send . renderHtmlBuilder >> flush

staticHandler :: ResponseHeaders -> FilePath -> Application
staticHandler mime fn request response = response $ responseFile status200 mime fn Nothing

notFoundHandler :: Int -> Application
notFoundHandler count request response = response $ responseLBS status200 [mimeText] "Unknown path"

checkHandler :: MyDb -> Int -> Application
checkHandler db count request response = response $ responseStream status200 [mimeJSON] (justCheckStreamJson db count)
     where
         justCheckStreamJson :: MyDb -> Int -> (Builder -> IO ()) -> IO () -> IO ()
         justCheckStreamJson db count send flush =
             return db >>= getUnreadCount >>= send . justUnreadCountToJson >> flush

updateHandler :: MyDb -> Int -> Application
updateHandler db count request response = response $ responseStream status200 [mimeJSON] (justFeedStreamJson db count)
    where
        justFeedStreamJson :: MyDb -> Int -> (Builder -> IO ()) -> IO () -> IO ()
        justFeedStreamJson db count send flush = do
          feedUrl <- getCachedFeed db count
          (feed, res) <- readApi feedUrl

          case res of
            Left err -> send $ justTweetsToJson $ Left err
            Right ts -> send $ justTweetsToJson $ Right ts

          flush

retweetHandler :: Int -> Application
retweetHandler count request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            print $ "got retweet " ++ show id_
            response $ responseStream status200 [mimeJSON] (retweetStream (fromIntegral int :: Int64))

          Nothing -> do
            print ("bad retweet id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad retweet id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        retweetStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        retweetStream id_ send flush =
            return (retweetUrl id_) >>= writeApi >>= send . retweetToJson >> flush


starHandler :: Int -> Application
starHandler count request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            print $ "got star " ++ show id_
            response $ responseStream status200 [mimeJSON] (starStream (fromIntegral int :: Int64))

          Nothing -> do
            print ("bad star id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad star id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        starStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        starStream id_ send flush =
            return (starUrl id_) >>= writeApi >>= send . starToJson >> flush

tweetHandler :: Int -> Application
tweetHandler count request response = case queryString request of
    [("status", Just status)] -> do
        print "got tweet "
        print status
        response $ responseStream status200 [mimeJSON] (tweetStream status)

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        tweetStream :: TweetBody -> (Builder -> IO ()) -> IO () -> IO ()
        tweetStream status send flush =
            return (tweetUrl status) >>= writeApi >>= send . tweetToJson >> flush
