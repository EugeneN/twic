{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.App where

import           Control.Monad.IO.Class
import           Control.Monad                 (forever, forM_)
import           Control.Exception             (fromException, handle)
import           Control.Concurrent            (MVar, newMVar, modifyMVar_, takeMVar, modifyMVar, readMVar
                                               , forkIO, threadDelay, killThread, myThreadId, ThreadId)
import           Network.Wai                   (responseStream, Application, pathInfo
                                               , responseLBS, responseFile, queryString)
import           Network.HTTP.Types            (status200, HeaderName)
import           Network.HTTP.Types.Header     (ResponseHeaders)
import qualified Network.WebSockets            as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import           Blaze.ByteString.Builder      (Builder, fromByteString)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import           Data.ByteString
import           Data.ByteString.Char8         (readInteger)
import qualified Data.ByteString.Lazy          as BSL
import           Data.Int                      (Int64)
import qualified Data.Text                      as T
import           Data.Text                      (Text)

import           UI.HTTP.Json                  (justTweetsToJson, justUnreadCountToJson)
import           UI.HTTP.Html                  (tweetsToHtml, retweetToHtml, justTweetsToHtml, homePage)
import           BL.Core                       (getCachedFeed, readApi, writeApi, retweetUrl, getUnreadCount)
import           BL.Types                      (TweetId, Message(..), Tweet)
import           BL.DataLayer                  (MyDb)
import           Config                        (heartbeatDelay)
import           Data.Aeson                    (encode)
import           Control.Applicative           ((<$>))
import           Data.Tuple
import           Data.UUID.V4                  (nextRandom)
import           Data.UUID                     (UUID)

--import qualified Network.Wai.Application.Static as Static
--import Data.FileEmbed (embedDir)

type Filename  = String
type Client    = (UUID, WS.Connection)
type WSState   = [Client]
type FeedState = [Tweet]

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

    path                -> notFoundHandler count request sendResponse

makeClient :: UUID -> WS.Connection -> Client
makeClient a c = (a, c)

addClient :: Client -> WSState -> WSState
addClient client clients = client : clients

removeClient :: Client -> WSState -> WSState
removeClient client = Prelude.filter ((/= fst client) . fst)

broadcast :: BSL.ByteString -> WSState -> IO ()
broadcast msg clients = forM_ clients $ \(_, conn) -> WS.sendTextData conn msg

app db m count = do
    cs <- newMVar ([] :: WSState)
    startBroadcastWorker m cs
    return $ WaiWS.websocketsOr WS.defaultConnectionOptions
                                (wsapp m cs)
                                (httpapp db count)

wsapp :: MVar FeedState -> MVar WSState -> WS.ServerApp
wsapp m cs pending = do
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

trackConnection :: Client -> MVar [Client] -> IO ()
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
             send $ justUnreadCountToJson unreadCount
             flush

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
    [("id", Just id_)] -> case readInteger id_ of
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
        retweetStream id_ send flush = writeApi (retweetUrl id_) >>= send . renderHtmlBuilder . retweetToHtml >> flush
