{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.App where

import           BL.Core                        (retweetUrl, starUrl, tweetUrl,
                                                 writeApi)
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
import           Prelude                        hiding (error)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Blaze.Html.Renderer.Utf8  (renderHtmlBuilder)
import           UI.HTTP.Html                   (homePage, justTweetsToHtml,
                                                 retweetToHtml, tweetsToHtml)
import           UI.HTTP.Json                   (justTweetsToJson,
                                                 justUnreadCountToJson,
                                                 retweetToJson, starToJson,
                                                 tweetToJson)



logRealm = "HttpApp"

info = infoM logRealm
warn = warningM logRealm
debug = debugM logRealm
error = errorM logRealm

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

httpapp :: MyDb -> Application -- = Request -> ResourceT IO Response
httpapp db request sendResponse = do
  debug $ show $ pathInfo request
  case pathInfo request of
    []                  -> homeHandler request sendResponse

    ["cs", "Main.js"]   -> staticHandler [mimeJs] "dist/cs/Main.js" request sendResponse
    ["favicon.ico"]     -> staticHandler [mimeIco] "res/favicon.ico" request sendResponse

    ["retweet"]         -> retweetHandler request sendResponse
    ["retweet", _]      -> retweetHandler request sendResponse

    ["star"]            -> starHandler request sendResponse
    ["star", _]         -> starHandler request sendResponse

    ["tweet"]           -> tweetHandler request sendResponse
    ["tweet", _]        -> tweetHandler request sendResponse

    path                -> notFoundHandler request sendResponse

makeClient :: UUID -> WS.Connection -> Client
makeClient a c = (a, c)

addClient :: Client -> WSState -> WSState
addClient client clients = client : clients

removeClient :: Client -> WSState -> WSState
removeClient client = Prelude.filter ((/= fst client) . fst)

broadcast :: BSL.ByteString -> WSState -> IO ()
broadcast msg clients = forM_ clients $ \(_, conn) -> WS.sendTextData conn msg

app :: MyDb -> MVar FeedState -> IO Application
app db m = do
    cs <- newMVar ([] :: WSState)
    _ <- startBroadcastWorker m cs
    return $ WaiWS.websocketsOr WS.defaultConnectionOptions
                                (wsapp m cs)
                                (httpapp db)

wsapp :: MVar FeedState -> MVar WSState -> WS.ServerApp
wsapp _ cs pending = do
  conn <- WS.acceptRequest pending
  clientId <- nextRandom
  let client = makeClient clientId conn

  debug $ "<<~~ Incoming ws connection " ++ show clientId

  WS.forkPingThread conn heartbeatDelay
  modifyMVar_ cs $ \cur -> return $ addClient client cur
  trackConnection client cs

startBroadcastWorker :: MVar FeedState -> MVar WSState -> IO ThreadId
startBroadcastWorker m cs = forkIO $
    forever $ do
        ts <- takeMVar m
        clients <- readMVar cs
        -- TODO if there are no clients, put ts back?
        broadcast (encode ts) clients

trackConnection :: Client -> MVar WSState -> IO ()
trackConnection client@(clientId, clientConn) cs = handle catchDisconnect $
  forever $ do
    x <- WS.receive clientConn
    debug $ "?>?> got smth from ws client " ++ show clientId ++ ": " ++ show x

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
        homeStream send flush = return homePage >>= send . renderHtmlBuilder >> flush

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
            return (retweetUrl id_) >>= writeApi >>= send . retweetToJson >> flush


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
            return (starUrl id_) >>= writeApi >>= send . starToJson >> flush

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
            return (tweetUrl status) >>= writeApi >>= send . tweetToJson >> flush
