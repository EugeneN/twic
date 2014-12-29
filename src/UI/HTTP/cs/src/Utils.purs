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


foreign import toString
    "function toString(a){                \
    \    console.log('toString:', a);     \
    \    return a.toString();             \
    \} " :: forall a. a -> String

foreign import readInt
    """function readInt(a){
        console.log('readInt:', a);
        var z = parseInt(a, 10);
        return z;
    } """ :: forall a. a -> Number

foreign import isNumeric
    """function isNumeric(a){
        return /^[0-9]+$/.test(a)
    } """ :: String -> Boolean


foreign import scrollToTop
    "function scrollToTop() {             \
    \       console.log('scroll to top'); \
    \       window.scroll(0,0);           \
    \       return undefined;             \
    \}" :: forall eff. (Eff (dom :: DOM | eff) Unit)

foreign import jsonStringify
    "function jsonStringify(o) {\
    \  return JSON.stringify(o);\
    \}" :: forall r. { | r} -> String

foreign import extractTarget
    "function extractTarget(ev){ \
    \    return ev.target;       \
    \} " :: J.JQueryEvent -> HTMLElement

foreign import extractCoords
    "function extractCoords(ev){            \
    \    return [ev.clientX, ev.clientY];   \
    \} " :: J.JQueryEvent -> [Number]

foreign import rioGet
    """
    function rioGet(url){
        return Rx.Observable.create(function(observer){
            var ok = function(result) { observer.onNext(JSON.stringify(result)) },
                nok = function(error) { observer.onError(JSON.stringify(error)) };
            jQuery.ajax(
                url,
                { type: 'GET'
                , success: ok
                , error: nok
                });
        });
    }
    """ :: forall a.Url -> Rx.Observable a

foreign import rioPost
    """
    function rioPost(url){
        return function(data){
            return Rx.Observable.create(function(observer){
                var ok = function(result) { observer.onNext(JSON.stringify(result)) },
                    nok = function(error) { observer.onError(JSON.stringify(error)) };
                jQuery.ajax(
                    url,
                    { type: 'POST'
                    , data: data
                    , success: ok
                    , error: nok
                    });
            });
        }
    }
    """ :: forall a b. Url -> b -> Rx.Observable a

oneSecond = 1000
oneMinute = 60 * oneSecond

foreign import getIntervalStream
    """
      function getIntervalStream(n) {
        return Rx.Observable.interval(n);
      }
    """ :: forall a. a -> Rx.Observable a

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

foreign import filterRx
    """
    function filterRx(f){
        return function(observable) {
            return observable.filter(f)
        }
    }
    """ :: forall a. (a -> Boolean) -> Rx.Observable a -> Rx.Observable a

foreign import byId
    """
    function byId(idstr){
        return function(event) {
            return event.target.id === idstr;
        }
    }
    """ :: forall a. String -> a -> Boolean

startAppBus state = do
    bodyClicks <- J.select "body" >>= onAsObservable "click"
    (filterRx (byId "load-new-tweets-id") bodyClicks) ~> \_ -> trace "click"

    pure unit

foreign import value
    "function value(el) {\
    \  return el.value;\
    \}" :: Element -> String

foreign import setFocus "function setFocus(id) { return function() { jQuery('#' + id).focus(); }}" :: forall eff. String -> Eff eff Unit
foreign import which "function which(ev) { return ev.which; }" :: forall a. a -> Number

data KeyCode = Insert | Escape | Enter | Delete | UnknownKey Number

keyEventToKeyCode :: forall a. a -> KeyCode -- JQueryEvent a, ReactEvent a =>
keyEventToKeyCode x | which x == 13 = Enter
keyEventToKeyCode x | which x == 27 = Escape
keyEventToKeyCode x | which x == 45 = Insert
keyEventToKeyCode x | which x == 46 = Delete

keyEventToKeyCode x                 = UnknownKey $ which x

instance eqKeyCode :: Eq KeyCode where
    (==) Insert Insert = true
    (==) Escape Escape = true
    (==) Enter  Enter  = true
    (==) Delete Delete = true
    (==) _      _      = false

    (/=) a      b      = not $ (==) a b

