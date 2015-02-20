{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.App where

import           BL.Core                             (getStatus, retweetUrl,
                                                      saveLastSeen, readHistory,
                                                      saveLastSeenAsync,
                                                      starUrl, tweetUrl,
                                                      updateFeed, writeApi)
import           BL.DataLayer                        (MyDb)
import           BL.Types                            (FeedState, Message (..),
                                                      Tweet, TweetBody, TweetId,
                                                      UpdateMessage)
import           Blaze.ByteString.Builder            (Builder, fromByteString)
import           Config                              (heartbeatDelay)
import           Control.Applicative                 ((<$>))
import           Control.Concurrent                  (MVar, ThreadId, forkIO,
                                                      killThread, modifyMVar,
                                                      modifyMVar_, myThreadId,
                                                      newMVar, putMVar,
                                                      readMVar, takeMVar,
                                                      threadDelay, tryTakeMVar)
import           Control.Exception                   (fromException, handle)
import           Control.Monad                       (forM_, forever)
import           Control.Monad.IO.Class
import           Data.Aeson                          (encode)
import           Data.ByteString
import qualified Data.ByteString.Char8               as B8
import qualified Data.ByteString.Lazy                as BSL
import           Data.Int                            (Int64)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Tuple
import           Data.UUID                           (UUID)
import           Data.UUID.V4                        (nextRandom)
import           Network.HTTP.Types                  (HeaderName, status200)
import           Network.HTTP.Types.Header           (ResponseHeaders)
import           Network.Wai                         (Application, pathInfo,
                                                      queryString, responseFile,
                                                      responseLBS,
                                                      responseStream)
import qualified Network.Wai.Handler.WebSockets      as WaiWS
import qualified Network.WebSockets                  as WS
import           Prelude                             hiding (error)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Blaze.Html.Renderer.Utf8       (renderHtmlBuilder)
import           UI.HTTP.Html                        (homePage,
                                                      justTweetsToHtml,
                                                      retweetToHtml,
                                                      tweetsToHtml)
import           UI.HTTP.Json                        (justTweetsToJson,
                                                      justUnreadCountToJson,
                                                      retweetToJson, starToJson,
                                                      tweetToJson)

import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Data.Time.Clock                     (UTCTime (..))



logRealm = "HttpApp"

info = infoM logRealm
warn = warningM logRealm
debug = debugM logRealm
error = errorM logRealm

type Filename  = String
type Client    = (UUID, WS.Connection)
type WSState   = [Client]


mimeHtml :: (HeaderName, ByteString)
mimeHtml = ("Content-Type", "text/html")
mimeText = ("Content-Type", "text/plain")
mimeJs   = ("Content-Type", "text/javascript")
mimeJSON = ("Content-Type", "application/json")
mimeIco  = ("Content-Type", "image/x-icon")

httpapp :: UTCTime -> MyDb -> Application -- = Request -> ResourceT IO Response
httpapp st db request sendResponse = do
  debug $ show $ pathInfo request
  case pathInfo request of
    []                  -> homeHandler request sendResponse

    ["cs", "Main.js"]   -> staticHandler [mimeJs] "dist/cs/Main.js" request sendResponse
    ["favicon.ico"]     -> staticHandler [mimeIco] "res/favicon.ico" request sendResponse

    path                -> case Prelude.head path of
        "retweet"       -> retweetHandler request sendResponse
        "star"          -> starHandler request sendResponse
        "tweet"         -> tweetHandler request sendResponse
        "stat"          -> statHandler st db request sendResponse
        "history"       -> historyHandler request sendResponse
        _               -> notFoundHandler request sendResponse

makeClient :: UUID -> WS.Connection -> Client
makeClient a c = (a, c)

addClient :: Client -> WSState -> WSState
addClient client clients = client : clients

removeClient :: Client -> WSState -> WSState
removeClient client = Prelude.filter ((/= fst client) . fst)

sendToClients :: MyDb -> MVar [Client] -> [Tweet] -> IO ()
sendToClients db cs ts = do
    clients <- readMVar cs
    case clients of
        [] -> info "No clients available"
        _ -> do
            saveLastSeenAsync db ts
            broadcast (encode ts) clients

broadcast :: BSL.ByteString -> WSState -> IO ()
broadcast msg clients = forM_ clients $ \(_, conn) -> WS.sendTextData conn msg

app :: UTCTime -> MyDb -> MVar FeedState -> MVar UpdateMessage -> IO Application
app st db fv uv = do
    cs <- newMVar ([] :: WSState)
    _ <- startBroadcastWorker db fv cs
    return $ WaiWS.websocketsOr WS.defaultConnectionOptions
                                (wsapp db fv uv cs)
                                (httpapp st db)

wsapp :: MyDb -> MVar FeedState -> MVar UpdateMessage -> MVar WSState -> WS.ServerApp
wsapp db fv uv cs pending = do
  conn <- WS.acceptRequest pending
  clientId <- nextRandom
  let client = makeClient clientId conn

  debug $ "<<~~ Incoming ws connection " ++ show clientId

  WS.forkPingThread conn heartbeatDelay
  modifyMVar_ cs $ \cur -> return $ addClient client cur
  updateFeed uv
  trackConnection client cs

startBroadcastWorker :: MyDb -> MVar FeedState -> MVar WSState -> IO ThreadId
startBroadcastWorker db fv cs = forkIO $
    forever $ do
        ts <- takeMVar fv
        sendToClients db cs ts

trackConnection :: Client -> MVar WSState -> IO ()
trackConnection client@(clientId, clientConn) cs = handle catchDisconnect $
  forever $ do
    x <- WS.receive clientConn
    debug $ "?>?> got smth from ws client " ++ show clientId ++ ": " ++ show x
    -- TODO handle ControlMessage (Close 1001 "")

    where
      catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> do
          warn $ "ws ConnectionClosed for " ++ show clientId
          filterOutClient client cs
          return ()

        Just (WS.CloseRequest a b) -> do
          warn $ "ws CloseRequest from client " ++ show clientId ++ ": " ++ show a ++ "/" ++ show b
          filterOutClient client cs
          return ()

        Just (WS.ParseException s) -> do
          error $ "ws ParseException for " ++ show clientId ++ ": " ++ s
          filterOutClient client cs
          return ()

        x -> do
          error $ "ws closed for unknown reason for " ++ show clientId ++ ": " ++ show x
          filterOutClient client cs
          return ()

      filterOutClient client cs =
        modifyMVar_ cs $ \clients ->
            return $ removeClient client clients

homeHandler :: Application
homeHandler request response = response $ responseStream status200 [mimeHtml] homeStream
    where
        homeStream :: (Builder -> IO ()) -> IO () -> IO ()
        homeStream send flush = (send . renderHtmlBuilder) homePage >> flush

staticHandler :: ResponseHeaders -> FilePath -> Application
staticHandler mime fn request response = response $ responseFile status200 mime fn Nothing

notFoundHandler :: Application
notFoundHandler request response = response $ responseLBS status200 [mimeText] "Unknown path"

retweetHandler :: Application
retweetHandler request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            debug $ "got retweet " ++ show id_
            response $ responseStream status200 [mimeJSON] (retweetStream (fromIntegral int :: Int64))

          Nothing -> do
            error ("bad retweet id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad retweet id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        retweetStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        retweetStream id_ send flush =
            writeApi (retweetUrl id_) >>= send . retweetToJson >> flush


starHandler :: Application
starHandler request response = case queryString request of
    [("id", Just id_)] -> case B8.readInteger id_ of
          Just (int, str) -> do
            debug $ "got star " ++ show id_
            response $ responseStream status200 [mimeJSON] (starStream (fromIntegral int :: Int64))

          Nothing -> do
            error ("bad star id" :: String)
            response $ responseLBS status200 [mimeJSON] "bad star id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        starStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        starStream id_ send flush =
            writeApi (starUrl id_) >>= send . starToJson >> flush

historyHandler :: Application
historyHandler request response = case queryString request of
    [("maxid", Just id_), ("count", Just count)] -> case B8.readInteger id_ of
          Just (int_id, str) -> do
            debug $ "got history request with maxid " ++ show int_id ++ " and count " ++ show count
            response $ responseStream status200 [mimeJSON] (historyStream (fromIntegral int_id :: Int64))

          Nothing -> do
            error ("bad history maxid" :: String)
            response $ responseLBS status200 [mimeJSON] "bad history id"

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        historyStream :: TweetId -> (Builder -> IO ()) -> IO () -> IO ()
        historyStream id_ send flush =
            readHistory id_ 20 >>= send . justTweetsToJson >> flush


tweetHandler :: Application
tweetHandler request response = case queryString request of
    [("status", Just status)] -> do
        debug "got tweet "
        debug $ show status
        response $ responseStream status200 [mimeJSON] (tweetStream status)

    _ -> response $ responseLBS status200 [mimeJSON] "bad request"

    where
        tweetStream :: TweetBody -> (Builder -> IO ()) -> IO () -> IO ()
        tweetStream status send flush =
            writeApi (tweetUrl status) >>= send . tweetToJson >> flush

statHandler :: UTCTime -> MyDb -> Application
statHandler st db request response = response $ responseStream status200 [mimeText] (respStream st db)
    where
    respStream :: UTCTime -> MyDb -> (Builder -> IO ()) -> IO () -> IO ()
    respStream st db send flush = do
        send (fromByteString "hello\n") >> flush
        getStatus st db >>=  send . fromByteString . B8.pack . show >> flush
