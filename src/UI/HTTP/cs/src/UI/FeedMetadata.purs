module UI.FeedMetadata where

import UI.Types
import Data.Monoid (mempty)
import Data.Maybe
import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified React.DOM as D
import React (createClass, renderComponentById, spec)
import React.Types (Component(), ComponentClass(),  React())
import Types
import Core (readState, writeState)
import Data.Array
import Control.Monad.Eff.Ref
import Utils
import Optic.Core ( (.~), (^.))
import Core
import UI.Feed (handleAuthorContextMenu)

handleBack :: forall eff. RefVal State -> Eff (ref :: Ref | eff) Unit
handleBack state = do
    s <- readState state
    writeState state (s # extraFeedL .~ Nothing)

feedMetadata :: ComponentClass { state :: RefVal State } {}
feedMetadata = createClass spec { displayName = "feedMetadata", render = renderFun } where
    renderFun this = do
        State { extraFeed = maybeEF } <- readState this.props.state

        case maybeEF of
            Just (BFeed { author = mbAuthor })  -> case mbAuthor of
                Nothing ->pure $
                    D.div {className: "feed-meta"} [
                        D.button { className: "user-icon"
                                 , style: { "border-radius": "50%"
                                          , "border": "0px"
                                          , "margin": "20px"
                                          , "cursor": "pointer"
                                          , "margin-right": "5px" }
                                 , onClick: handleBack this.props.state } [D.rawText "←"]
                        ]

                Just a@(Author { name = n
                               , screen_name = sn
                               , profile_image_url = avatar}) -> pure $
                    D.div {className: "feed-meta"} [
                        D.button { className: "user-icon"
                                 , style: { "border-radius": "50%"
                                          , "border": "0px"
                                          , "margin": "20px"
                                          , "cursor": "pointer"
                                          , "margin-right": "5px" }
                                 , onClick: handleBack this.props.state } [D.rawText "←"]

                      , D.span { className: "user-icon"
                               , style: { "margin": "20px"
                                        , "margin-left": "5px"
                                        , "display": "inline-block" } } [
                            D.a {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                                D.img { className: "user-icon-img"
                                      , src: avatar
                                      , onContextMenu: (callEventHandler $ handleAuthorContextMenu this.props.state a)
                                      , title: n} [] ] ] ]

            Nothing -> pure $ D.div {} []
