module Utils where

import Debug.Trace
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Ref
import DOM (DOM(..))
import qualified Control.Monad.JQuery as J
import Data.DOM.Simple.Types (HTMLElement(..))
import Data.DOM.Simple.Element (getAttribute)

import qualified Rx.Observable as Rx
import Rx.JQuery

import Types

import Data.Either
import Data.Monoid
import Data.Array (length, reverse)
import React.Types (React(), Element())

import qualified Network.XHR as X
import qualified Network.XHR.Internal as XI
import qualified Network.XHR.Types as XT

foreign import setTitle :: forall eff. String -> Eff (dom :: DOM | eff) Unit

foreign import toString :: forall a. a -> String

foreign import readInt :: forall a. a -> Number

foreign import isNumeric :: String -> Boolean


foreign import scrollToTop :: forall eff. (Eff (dom :: DOM | eff) Unit)

foreign import jsonStringify :: forall r. { | r} -> String

foreign import extractTarget :: J.JQueryEvent -> HTMLElement

foreign import extractCoords :: J.JQueryEvent -> Array Number

foreign import rioGet :: forall a.Url -> Rx.Observable a

foreign import rioPost :: forall a b. Url -> b -> Rx.Observable a

oneSecond = 1000
oneMinute = 60 * oneSecond

foreign import getIntervalStream :: forall a. a -> Rx.Observable a

(~>) :: forall eff a. Rx.Observable a -> (a -> Eff eff Unit) -> Eff eff Unit
(~>) = Rx.subscribe


rxTest :: forall e. Eff (trace :: Trace, dom :: DOM | e) Unit
rxTest = do
    spansStream <- J.select "span" >>= onAsObservable "mouseover"
    imgsStream <- J.select "img" >>= onAsObservable "mouseover"

    let s = spansStream <> imgsStream
--    s ~> (toString >>> trace)

    let s2 = extractCoords <$> s
    s2 ~> (toString >>> trace)

foreign import filterRx :: forall a. (a -> Boolean) -> Rx.Observable a -> Rx.Observable a

foreign import byId :: forall a. String -> a -> Boolean

startAppBus state = do
    bodyClicks <- J.select "body" >>= onAsObservable "click"
    (filterRx (byId "load-new-tweets-id") bodyClicks) ~> \_ -> trace "click"

    pure unit

foreign import value :: Element -> String

foreign import setFocus :: forall eff. String -> Eff eff Unit
foreign import which :: forall a. a -> Number

foreign import scrollToEl :: forall eff. String -> (Eff (dom :: DOM | eff) Unit)

foreign import data Timeout :: !

foreign import forkPostpone :: forall a eff. a -> Number -> Eff (|eff) Unit

foreign import splitAt :: forall a. Array a -> Number -> Array (Array a)


foreign import getDeltaY :: forall a. a -> Number

foreign import bufferWithTime :: forall a. Number -> Rx.Observable a -> Rx.Observable (Array a)

foreign import throttleWithTimeout :: forall a. Number -> Rx.Observable a -> Rx.Observable a

foreign import getWheelObservable :: forall a b. b -> Rx.Observable a

foreign import getNewObservable :: forall a b. a -> Rx.Observable b

foreign import publishToObservable :: forall a b. Rx.Observable a -> b -> Rx.Observable a

foreign import setProps :: forall a eff. a -> RefVal State ->  Eff (dom :: DOM, react :: React | eff) Unit

data KeyCode = Insert
             | Escape
             | Enter
             | Delete
             | F1
             | F2
             | F3
             | F4
             | F5
             | UnknownKey Number

keyEventToKeyCode :: forall a. a -> KeyCode -- JQueryEvent a, ReactEvent a =>
keyEventToKeyCode x | which x == 13  = Enter
                    | which x == 27  = Escape
                    | which x == 45  = Insert
                    | which x == 46  = Delete
                    | which x == 112 = F1
                    | which x == 113 = F2
                    | which x == 114 = F3
                    | which x == 115 = F4
                    | which x == 116 = F5

keyEventToKeyCode x                  = UnknownKey $ which x

instance eqKeyCode :: Eq KeyCode where
    (==) Insert Insert = true
    (==) Escape Escape = true
    (==) Enter  Enter  = true
    (==) Delete Delete = true
    (==) F1     F1     = true
    (==) F2     F2     = true
    (==) F3     F3     = true
    (==) F4     F4     = true
    (==) F5     F5     = true
    (==) _      _      = false

    (/=) a      b      = not $ (==) a b



instance eqUUID :: Eq UUID where
    (==) ident ident' = showuuid ident == showuuid ident'
    (/=) ident ident' = not (ident == ident')

instance showUUID :: Show UUID where
    show ident = showuuid ident

foreign import showuuid :: UUID -> String

foreign import runUUID :: Eff (uuid :: UUIDEff) UUID -> UUID

foreign import getUUID :: forall eff. Eff (uuid :: UUIDEff | eff) UUID

foreign import callEventHandler :: forall f e eff. f -> e -> Eff ( | eff) Unit

foreign import stopPropagation :: forall a b. a -> Eff ( | b) Unit

foreign import stringReplace :: String -> String -> String -> String