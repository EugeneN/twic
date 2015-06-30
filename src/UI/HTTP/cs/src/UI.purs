module UI where

import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import Debug.Trace
import React
    ( createClass
    , eventHandler
    , renderComponentById
    , spec
    )
import React.Types
    ( Component()
    , ComponentClass()
    , Event()
    , React()
    , ReactFormEvent()
    , ReactThis()
    )

import Core
import Utils
import Types
import Data.Monoid
import Data.Foldable
import Data.Maybe
import Data.Array
import qualified Data.String as S

import qualified React.DOM as D

class AsHtml a where
    asHtml :: a -> Component

instance asHtmlTweetElement :: AsHtml TweetElement where
    asHtml (AtUsername s)   = D.span {className: "username-tag"} [
                                  D.a {href: ("https://twitter.com/" ++ s), target: "_blank" } [D.rawText $ "@" ++ s]]

    asHtml (Link s)         = D.a {className: "inline-link", target: "_blank", href: s} [D.rawText $ linkToText s]
        where
            linkToText u = case (S.split "/" u) !! 2 of
                Nothing -> u
                Just x  -> x

    asHtml (PlainText s)    = D.span {className: "text-tag", dangerouslySetInnerHTML: {__html: s}} []

    asHtml (Hashtag s)      = D.span {className: "hash-tag"} [
                                D.a {href: ("https://twitter.com/hashtag/" ++ s ++ "?src=hash"), target: "_blank"}
                                    [D.rawText $ "#" ++ s]]

    asHtml (Retweet s)      = D.span {className: "retweet-tag"} [D.rawText "RT"]

    asHtml (Spaces s)       = D.span {} [D.rawText s]

    asHtml (Unparsable s)   = D.span {className: "unparsable"} [D.rawText s]

instance asHtmlTweet :: AsHtml Tweet where
    asHtml (Tweet { text = t , created_at = c , id = i , id_str = s , user = u , entities = e , retweet = Nothing }) =
        tweetComponent {text: t, created_at: c, id: i, id_str: s, author: u, entities: e, retweeted_by: Nothing} []

    asHtml (Tweet { created_at = c , id = i , id_str = s , user = u , retweet = Just (
            Tweet { text = origText , created_at = origCreatedAt , id = origId
                  , id_str = origIdString, entities = origEntities, user = origAuthor}) }) =
        tweetComponent {text: origText, created_at: c, id: i, id_str: s, author: u, entities: origEntities, retweeted_by: Just origAuthor} []


instance asHtmlAuthor :: AsHtml Author where
    asHtml (Author {name = n, screen_name = sn, profile_image_url = avatar})
        = D.span {className: "user-icon"} [
            D.a {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                D.img {className: "user-icon-img", src: avatar, title: n} []]]

instance asHtmlEntities :: AsHtml Entities where
    asHtml (Entities { urls = us
                     , hashtags = hs
                     , media = mms })
        = case mms of
            Just ms -> D.div {className: "media"} (asHtml <$> ms)
            Nothing -> D.div {className: "media"} []

instance asHtmlMedia :: AsHtml EntityMedia where
    asHtml (EntityMedia { mType = type_
                        , mMediaUrl = url
                        })
        = case type_ of
            "photo" -> D.img {className: "inline-img", src: url} []
            x -> D.div {className: "unknown-media"} [D.rawText "Unknown media"]


handleRetweetClick id_ = do
    let url = "/retweet/?id=" ++ id_
    (rioPost url Nothing) ~> retweetResultHandler
    pure unit

    where
        retweetResultHandler :: forall e. AjaxResult -> Eff (trace :: Trace | e) Unit
        retweetResultHandler resp = do
            trace $ "retweeted " ++ show resp
            pure unit

handleStarClick id_ = do
    let url = "/star?id=" ++ id_
    (rioPost url Nothing) ~> starResultHandler
    pure unit

    where
        starResultHandler :: forall e. AjaxResult -> Eff (trace :: Trace | e) Unit
        starResultHandler resp = do
            trace $ "starred " ++ show resp
            pure unit

messageComponent :: ComponentClass {message :: String} {}
messageComponent = createClass spec { displayName = "Message", render = renderFun } where
    renderFun this = pure $ D.div {className: "message"} [D.rawText this.props.message]

checkButtonComponent :: ComponentClass {count :: Number} {}
checkButtonComponent = createClass spec { displayName = "CheckButton", render = renderFun } where
    renderFun this = pure $
        D.button {className: aClass, id: "load-new-tweets-id"} [D.rawText $ show this.props.count]
        where
            aClass = case this.props.count of
               0 -> "no-new-tweets pop"
               _ -> "there-are-new-tweets pop"

showLoaderComponent :: ComponentClass {} {}
showLoaderComponent = createClass spec { displayName = "Loader", render = renderFun } where
                      renderFun this = pure $ D.div {className: "no-tweets"} [
                                                D.img {src: "http://eugenen.github.io/resources/public/img/loading1.gif"} []]

hideLoaderComponent :: ComponentClass {} {}
hideLoaderComponent = createClass spec { displayName = "Loader", render = renderFun } where
                      renderFun this = pure $ D.div {className: "no-tweets"} []

process :: Entities -> TweetElement -> TweetElement
process (Entities {urls=urls}) (Link u) = case matchUrl u of
    Nothing -> Link u
    Just (EntityUrl {eExpandedUrl = xu}) -> Link xu

    where
        matchUrl url = case filterUrl url of
         [x] -> Just x
         _   -> Nothing

        filterUrl url = filter (f u) urls
        f u (EntityUrl {eUrl = eUrl}) = eUrl == u

process entities x = x

getOrigTweetUrl :: Author -> String -> String
getOrigTweetUrl (Author {screen_name = screen_name}) tweetId = "https://twitter.com/" ++ screen_name ++ "/status/" ++ tweetId

tweetComponent :: ComponentClass {text :: Array TweetElement
                                 , created_at :: String
                                 , id :: TweetId
                                 , id_str   :: String
                                 , author :: Author
                                 , entities :: Entities
                                 , retweeted_by :: Maybe Author} {}
tweetComponent = createClass spec { displayName = "Tweet" , render = renderFun }
    where
        authorToHtml a Nothing = asHtml a
        authorToHtml (Author {name = name, screen_name = sn, profile_image_url = avatar})
                        (Just ((Author {name = origName, screen_name = origSn, profile_image_url = origAvatar}))) =
            D.span {className: "user-icon"} [
                D.span {className: "user-icon2"} [
                    D.a {href: "https://twitter.com/" ++ origSn, target: "_blank"} [
                        D.img {className: "user-icon-img", src: origAvatar, title: "Original author: " ++ origName} []]]
              , D.span {className: "user-icon1"} [
                    D.a {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                        D.img {className: "user-icon-img", src: avatar, title: name} []]]
              ]

        renderFun this = pure $
          D.li {} [ authorToHtml this.props.author this.props.retweeted_by
                  , D.span {className: "tweet-body"} (asHtml <<< (process this.props.entities) <$> this.props.text)

                  , asHtml this.props.entities

                  , D.span {className: "toolbar-target"} [
                        D.ul {className: "toolbar", id: "menu-" ++ (show this.props.id)} [
                            D.li { "data-tweet-id": (show this.props.id)
                                 , title: "Retweet"
                                 , onClick: handleRetweetClick this.props.id_str} [D.rawText "RT"]
                          , D.li {} [D.a {href: (getOrigTweetUrl this.props.author this.props.id_str)
                                         , target: "_blank"
                                         , title: "View original"} [D.rawText "⌘"]]
                          , D.li {title: "Reply"} [D.rawText "↩"]
                          , D.li { title: "Star"
                                 , onClick: handleStarClick this.props.id_str } [D.rawText "★"]
                          , D.li {title: "Mark"} [D.rawText "⚑"] ] ] ]



tweetsList :: ComponentClass {tweets :: Array Tweet} {}
tweetsList = createClass spec { displayName = "TweetsList", render = renderFun }
    where
        renderFun this = case this.props.tweets of
            [] -> pure $ D.ul {id: "feed"} [D.li { className: "no-tweets" } [D.rawText "No new tweets"]]
            _  -> pure $ D.ul {id: "feed"} $ asHtml <$> this.props.tweets

renderTweets :: forall eff. String -> Array Tweet -> Eff (dom :: DOM, react :: React | eff) Component
renderTweets targetId ts = renderComponentById (tweetsList {tweets: ts} []) targetId

renderMessage :: forall eff. String -> String -> Eff (dom :: DOM, react :: React | eff) Component
renderMessage targetId m = renderComponentById (messageComponent {message: m} []) targetId

showLoader :: forall eff. String -> Eff (dom :: DOM, react :: React | eff) Component
showLoader targetId = renderComponentById (showLoaderComponent {} []) targetId

hideLoader :: forall eff. String -> Eff (dom :: DOM, react :: React | eff) Component
hideLoader targetId = renderComponentById (hideLoaderComponent {} []) targetId

renderCheckButton :: forall eff. String -> Number -> Eff (dom :: DOM, react :: React | eff) Component
renderCheckButton targetId count = renderComponentById (checkButtonComponent {count: count} []) targetId