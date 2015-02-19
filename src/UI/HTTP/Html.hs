{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module UI.HTTP.Html where

import           Data.List.Split             (splitOn)
import           Data.Text                   (Text, append, pack, unpack)
import qualified Data.Text                   as T
import           Text.Blaze                  (toValue)
import           Text.Blaze.Html5            (Html, a, body, docTypeHtml, img,
                                              li, link, meta, pre, style, title,
                                              toHtml, ul, (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes (charset, class_, href, rel, src,
                                              target)
import qualified Text.Blaze.Html5.Attributes as A

import           Control.Monad               (forM_)
import           Data.Functor                ((<$>))
import           Data.Monoid                 (mconcat, mempty, (<>))

import           BL.Core
import qualified UI.HTTP.Css                 as CSS

import           Debug.Trace                 (trace)
import           Network.HTTP.Conduit        (HttpException)



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
      chomp = T.unpack . T.takeWhile (/= '/') . T.pack

  asHtml (PlainText t)  = H.span ! class_ "text-tag" $ H.preEscapedToHtml t -- XXX
  asHtml (Hashtag h)    = H.span ! class_ "hash-tag" $
                            H.a ! href (toValue ("https://twitter.com/hashtag/" ++ h ++ "?src=hash"))
                                ! target "_blank" $ toHtml ("#" ++ h)

  asHtml Retweet        = H.span ! class_ "retweet-tag" $ "RT"
  asHtml (Spaces s)     = H.span $ toHtml s
  asHtml (Unparsable t) = H.span ! class_ "unparsable" $ H.preEscapedToHtml t

instance AsHtml EntityMedia where
  asHtml (EntityMedia type_ _ _ mediaUrl _ _ _) | type_ == "photo"
    = img ! class_ "inline-img" ! src (toValue mediaUrl)
  asHtml (EntityMedia type_ _ _ _ _ _ _)
    = div_ ! class_ "unknown-media" $ toHtml ("Unknown media type: " ++ type_)

instance AsHtml Tweet where
  asHtml (Tweet body created id_ id_str (Author username authorId screen_name hasAvatar avatarUrl) entities retweet) = do
    span_ ! class_ "user-name" $ asHtml $ AtUsername (unpack username)
    span_ ! class_ "user-icon" $
        img ! class_ "user-icon-img" ! src (toValue avatarUrl)
            ! A.alt (toValue username)
            ! A.title (toValue username)
    span_ ! class_ "tweet-body" $ formatBody body

    span_ ! class_ "toolbar-target" $
        ul ! class_ "toolbar" ! A.id (toValue $ "menu-" ++ show id_) $ do
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
            Link url -> case matchUrl url of
              Nothing -> Link url
              Just (EntityUrl expandedUrl _ _ _) -> toLink $ splitOn "://" expandedUrl
                where
                    toLink [x,y] = Link $ x ++ "://" ++ y
                    toLink _ = Link url

            x -> x

            where
              matchUrl url = case filterResult url of
                [x] -> Just x
                _   -> Nothing

                where
                  filterResult u = filter (f u) urlEntities
                  f u (EntityUrl _ eUrl _ _) = eUrl == u


instance AsHtml (ApiError HttpException) where
  asHtml (ApiError s) = H.span ! class_ "error" $ toHtml s
  asHtml (TransportError x) = H.span ! class_ "error" $ toHtml $ show x



htmlPage :: Maybe Html -> Maybe Html -> Maybe Html -> Html
htmlPage title_ body_ error_ = docTypeHtml $ do
  head_ $ do
    meta ! charset "utf-8"
    link ! href "http://fonts.googleapis.com/css?family=Roboto:400,100,100italic,300,300italic,400italic,500,500italic,700,700italic,900,900italic&subset=cyrillic-ext,latin,greek-ext,greek,latin-ext,cyrillic"
         ! rel "stylesheet"
         ! A.type_ "text/css"

    case title_ of
      Nothing -> ""
      Just t  -> title t
    style $ toHtml CSS.allCss

  body ! A.id "body" $ do
    div_ ! A.id "root" $ mempty
    H.script ! A.src "/cs/Main.js" ! A.type_ "text/javascript" $ mempty

retweetToHtml :: Either (ApiError HttpException) Tweet -> Html
retweetToHtml res = case res of
  Left err -> htmlPage (Just "* Error loading tweets") Nothing (Just $ asHtml err)
  Right t  -> htmlPage (Just "retweet ok") (Just $ asHtml t) (Just $ H.span ! class_ "success" $ "retweet ok")

justTweetsToHtml :: Either (ApiError HttpException) [Tweet] -> Html
justTweetsToHtml (Left exp) = toHtml ("can't load" :: String)
justTweetsToHtml (Right ts) = case ts of
                               [] -> li ! class_ "no-tweets" $ "No new tweets"
                               _  -> forM_ (reverse ts) (li . asHtml)

tweetsToHtml :: Either (ApiError HttpException) [Tweet] -> Html
tweetsToHtml (Left exp) = htmlPage (Just "* Error loading tweets") Nothing (Just $ asHtml exp)
tweetsToHtml (Right ts) = htmlPage title_ body_ err_
  where
    title_ = Just $ toHtml $ show (length ts) ++ " new tweets"
    body_ = Just $ case ts of
             [] -> li ! class_ "no-tweets" $ "No new tweets"
             _  -> forM_ (reverse ts) (li . asHtml)
    err_ = Nothing

homePage :: Html
homePage = htmlPage title_ body_ err_
    where
        title_ = Just "Hello"
        body_ = Nothing
        err_ = Nothing
