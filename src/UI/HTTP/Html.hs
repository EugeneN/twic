{-# LANGUAGE OverloadedStrings #-}

module UI.HTTP.Html where

import           Text.Blaze.Html5              (Html, docTypeHtml, toHtml, (!), body, ul,
                                                style, title, li, img, a, meta, pre, link)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   (class_, src, href, charset, target, rel)
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze                    (toValue)
import           Data.Text                     (Text, pack, unpack, append)
import qualified Data.Text                     as T
import           Data.List.Split               (splitOn)

import           Data.Monoid                   ((<>), mconcat, mempty)
import           Data.Functor                  ((<$>))
import           Control.Monad                 (forM_)

import           BL.Core
import qualified UI.HTTP.Css                   as CSS

import           Debug.Trace                   (trace)



span_ = H.span
head_ = H.head
div_ = H.div

class AsHtml a where
    asHtml :: a -> Html

instance AsHtml TweetElement where
  asHtml (AtUsername u) = H.span ! class_ "username-tag" $ 
                            H.a ! href (toValue ("https://twitter.com/" ++ u))
                                ! target "_blank" $ toHtml ("@" ++ u)

  asHtml (Link u)     = H.a ! class_ "inline-link"
                              ! href (toValue u)
                              ! target "_blank" $ toHtml $ chomp u
    where
      chomp = T.unpack . T.takeWhile (\c -> c /= '/') . T.pack

  asHtml (PlainText t)  = H.span ! class_ "text-tag" $ H.preEscapedToHtml t -- XXX
  asHtml (Hashtag h)    = H.span ! class_ "hash-tag" $ 
                            H.a ! href (toValue ("https://twitter.com/hashtag/" ++ h ++ "?src=hash"))
                                ! target "_blank" $ toHtml ("#" ++ h)

  asHtml Retweet        = H.span ! class_ "retweet-tag" $ "RT"
  asHtml (Spaces s)     = H.span $ toHtml s
  asHtml (Unparsable t) = H.span ! class_ "unparsable" $ H.preEscapedToHtml t

instance AsHtml EntityMedia where
  asHtml (EntityMedia type_ _ _ mediaUrl _ _ _) = case type_ of
    "photo" -> img ! class_ "inline-img" ! src (toValue mediaUrl)
    x -> div_ ! class_ "unknown-media" $ toHtml ("Unknown media type: " ++ x)
  asHtml _ = ""

instance AsHtml Tweet where
  asHtml (Tweet body created id_ (Author username authorId screen_name hasAvatar avatarUrl) entities) = do
    span_ ! class_ "user-name" $ asHtml $ AtUsername (unpack username) 
    span_ ! class_ "user-icon" $ 
        img ! class_ "user-icon-img" ! src (toValue avatarUrl)
            ! A.alt (toValue username)
            ! A.title (toValue username)
    span_ ! class_ "tweet-body" $ formatBody body

    span_ ! class_ "toolbar-target" $
        ul ! class_ "toolbar" ! A.id (toValue $ "menu-" ++ (show id_)) $ do
            li ! H.customAttribute "data-tweet-id" (toValue id_)
               ! A.onclick (toValue ("PS.Main.handleRetweetButton(this)()" :: String)) $ "RETWEET"
            li "REPLAY"
            li "STAR"
            li "MARK"

    case media entities of
      Nothing -> ""
      Just ms -> mconcat $ asHtml <$> ms

    where
      formatBody :: [TweetElement] -> Html
      formatBody b = htmlBody
        where
          htmlBody            = mconcat htmlChunks
          htmlChunks          = asHtml <$> mappedTweetElements
          mappedTweetElements = processElement <$> b

          urlEntities         = urls entities
          processElement el   = case el of
            Link url -> case (matchUrl url) of
              Nothing -> Link url
              Just (EntityUrl expandedUrl _ _ _) -> toLink $ splitOn "://" expandedUrl
                where
                    toLink (x:y:[]) = Link $ x ++ "://" ++ y
                    toLink _ = Link url

            x -> x

            where
              matchUrl url = case (filterResult url) of
                [x] -> Just x
                _   -> Nothing

                where
                  filterResult u = filter (f u) urlEntities
                  f u (EntityUrl _ eUrl _ _) = eUrl == u


instance AsHtml ApiError where
  asHtml (ApiError s) = H.span ! class_ "error" $ toHtml s



htmlPage :: Maybe Html -> Maybe Html -> Maybe Html -> Html
htmlPage title_ body_ error_ = docTypeHtml $ do
  head_ $ do
    meta ! charset "utf-8"
    link ! href "http://fonts.googleapis.com/css?family=Roboto:400,100,100italic,300,300italic,400italic,500,500italic,700,700italic,900,900italic&subset=cyrillic-ext,latin,greek-ext,greek,latin-ext,cyrillic"
         ! rel "stylesheet"
         ! A.type_ "text/css"
    case title_ of
      Nothing -> ""
      Just t  -> title $ t
    style $ toHtml $ CSS.allCss

  body $ do
    div_ ! class_ "error" ! A.id "messages" $ case error_ of
                              Nothing -> mempty
                              Just exp -> div_ ! class_ "error" $ exp 

    div_ ! class_ "container" ! A.id "container" $ do
      case body_ of
          Nothing -> ""
          Just b -> ul ! A.id "feed" $ do b

    div_ ! class_ "refresh" ! A.id "refresh" $ do
      H.button ! class_ "there-are-new-tweets"
               ! A.id "load-new-tweets-id"
               $ ">>="

    H.script ! A.src "/cs/Main.js" ! A.type_ "text/javascript" $ mempty

retweetToHtml :: Either ApiError Tweet -> Html
retweetToHtml res = case res of
  Left err -> htmlPage (Just "* Error loading tweets") Nothing (Just $ asHtml err)
  Right t  -> htmlPage (Just "retweet ok") (Just $ asHtml t) (Just $ H.span ! class_ "success" $ "retweet ok")

justTweetsToHtml :: Either ApiError [Tweet] -> Html
justTweetsToHtml (Left exp) = toHtml ("can't load" :: String)
justTweetsToHtml (Right ts) = case ts of
                               [] -> li ! class_ "no-tweets" $ "No new tweets"
                               _  -> forM_ (reverse ts) (li . asHtml)

tweetsToHtml :: Either ApiError [Tweet] -> Html
tweetsToHtml (Left exp) = htmlPage (Just "* Error loading tweets") Nothing (Just $ asHtml exp)
tweetsToHtml (Right ts) = htmlPage title_ body_ err_
  where
    title_ = Just $ toHtml $ (show $ length ts) ++ " new tweets"
    body_ = Just $ case ts of
             [] -> li ! class_ "no-tweets" $ "No new tweets" 
             _  -> forM_ (reverse ts) (li . asHtml)
    err_ = Nothing

homePage :: Html
homePage = htmlPage title_ body_ err_
    where
        title_ = Just "Hello"
        body_ = Just $ li ! class_ "no-tweets" $ "Loading tweets"
        err_ = Nothing