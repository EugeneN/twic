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


foreign import setTitle
    "function setTitle(a){       \
    \  return function(){        \
    \    document.title = a;     \
    \    return undefined;       \
    \  }                         \
    \}" :: forall eff. String -> Eff (dom :: DOM | eff) Unit


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

    asHtml (PlainText s)    = D.span {className: "text-tag"} [D.rawText s]

    asHtml (Hashtag s)      = D.span {className: "hash-tag"} [
                                D.a {href: ("https://twitter.com/hashtag/" ++ s ++ "?src=hash"), target: "_blank"}
                                    [D.rawText $ "#" ++ s]]

    asHtml (Retweet s)      = D.span {className: "retweet-tag"} [D.rawText "RT"]

    asHtml (Spaces s)       = D.span {} [D.rawText s]

    asHtml (Unparsable s)   = D.span {className: "unparsable"} [D.rawText s]

instance asHtmlTweet :: AsHtml Tweet where
    asHtml (Tweet { text = t
                  , created_at = c
                  , id = i
                  , user = u
                  , entities = e
                  }) 
        = tweetComponent {text: t, created_at: c, id: i, author: u, entities: e} []

instance asHtmlAuthor :: AsHtml Author where
    asHtml (Author {name = n, profile_image_url = avatar}) 
        = D.span {className: "user-icon"} [D.img {className: "user-icon-img", src: avatar, title: n} []]

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
    trace $ "click" ++ id_
    retweet id_
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
               0 -> "no-new-tweets"
               _ -> "there-are-new-tweets"

loaderComponent :: ComponentClass {} {}
loaderComponent = createClass spec { displayName = "Loader", render = renderFun } where
                      renderFun this = pure $ D.div {className: "no-tweets"} [
                        D.img {src: "http://eugenen.github.io/resources/public/img/loading1.gif"} []]

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

getOrigTweetUrl :: Author -> TweetId -> String
getOrigTweetUrl (Author {screen_name = screen_name})  tweetId = "https://twitter.com/" ++ screen_name ++ "/status/" ++ show tweetId

tweetComponent :: ComponentClass {text :: [TweetElement], created_at :: String, id :: TweetId, author :: Author, entities :: Entities} {}
tweetComponent = createClass spec { displayName = "Tweet" , render = renderFun }
    where
        renderFun this = pure $
          D.li {} [ asHtml this.props.author 
                  , D.span {className: "tweet-body"} (asHtml <<< (process this.props.entities) <$> this.props.text)

                  , asHtml this.props.entities

                  , D.span {className: "toolbar-target"} [
                        D.ul {className: "toolbar", id: "menu-" ++ (show this.props.id)} [
                            D.li { "data-tweet-id": (show this.props.id)
                                 , onClick: handleRetweetClick (show this.props.id)} [D.rawText "RT"]
                          , D.li {} [D.a {href: (getOrigTweetUrl this.props.author this.props.id), target: "_blank"} [D.rawText "ORIG"]]
                          , D.li {} [D.rawText "RE"]
                          , D.li {} [D.rawText "★"]
                          , D.li {} [D.rawText "⚑"] ] ] ]

tweetsList :: ComponentClass {tweets :: [Tweet]} {}
tweetsList = createClass spec { displayName = "TweetsList", render = renderFun }
    where
        renderFun this = case this.props.tweets of
            [] -> pure $ D.ul {id: "feed"} [D.li { className: "no-tweets" } [D.rawText "No new tweets"]]
            _  -> pure $ D.ul {id: "feed"} $ asHtml <$> (reverse this.props.tweets)

renderTweets :: forall eff. String -> [Tweet] -> Eff (dom :: DOM, react :: React | eff) Component
renderTweets targetId ts = renderComponentById (tweetsList {tweets: ts} []) targetId

renderMessage :: forall eff. String -> String -> Eff (dom :: DOM, react :: React | eff) Component
renderMessage targetId m = renderComponentById (messageComponent {message: m} []) targetId

renderLoader :: forall eff. String -> Eff (dom :: DOM, react :: React | eff) Component
renderLoader targetId = renderComponentById (loaderComponent {} []) targetId

renderCheckButton :: forall eff. String -> Number -> Eff (dom :: DOM, react :: React | eff) Component
renderCheckButton targetId count = renderComponentById (checkButtonComponent {count: count} []) targetId