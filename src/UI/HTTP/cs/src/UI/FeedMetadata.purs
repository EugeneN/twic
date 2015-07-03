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

handleBack :: forall eff. Ref State -> Eff (ref :: REF | eff) Unit
handleBack state = do
    s <- readState state
    writeState state (s # extraFeedL .~ Nothing)

feedMetadata :: ComponentClass { state :: REFVal State } {}
feedMetadata = mkUI $ spec {} 	his -> do where
    renderFun this = do
        State { extraFeed = maybeEF } <- readState this.props.state

        case maybeEF of
            Just (BFeed { author = mbAuthor })  -> case mbAuthor of
                Nothing -> pure $
                    D.div [ P.className "feed-meta"n] [
                        D.button [ P.className "user-icon"
                                 , P.style { "border-radius": "50%"
                                          , "border": "0px"
                                          , "margin": "20px"
                                          , "cursor": "pointer"
                                          , "margin-right": "5px" }
                                 , P.onClick handleBack this.props.state ] [D.text "←"]
                      , D.span [ P.className "popup-panel-label"] [D.text "Search results"]
                      ]

                Just a@(Author { name = n
                               , screen_name = sn
                               , profile_image_url = avatar}) -> pure $
                    D.div [ P.className "feed-meta"} [
                        D.button [ P.className "user-icon"
                                 , P.style { "border-radius": "50%"
                                          , "border": "0px"
                                          , "margin": "20px"
                                          , "cursor": "pointer"
                                          , "margin-right": "5px" }
                                 , P.onClick handleBack this.props.state ] [D.text "←"]

                      , D.span [ P.className "user-icon"
                               , P.style { "margin": "20px"
                                        , "margin-left": "5px"
                                        , "display": "inline-block" } ] [
                            D.a {href: "https://twitter.com/" ++ sn, target: "_blank"} [
                                D.img { P.className "user-icon-img"
                                      , src: avatar
                                      , P.onDoubleClick: (callEventHandler $ handleAuthorContextMenu this.props.state a)
                                      , title: n} [] ] ] ]

            Nothing -> pure $ D.div {} []
