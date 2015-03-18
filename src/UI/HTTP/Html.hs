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

homePage :: Html
homePage = htmlPage title_ body_ err_
    where
        title_ = Just "Hello"
        body_ = Nothing
        err_ = Nothing
