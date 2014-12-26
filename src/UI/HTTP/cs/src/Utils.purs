module Utils where

import Control.Monad.Eff (Eff(..))
import DOM (DOM(..))
import qualified Control.Monad.JQuery as J
import Data.DOM.Simple.Types (HTMLElement(..))
import Data.DOM.Simple.Element (getAttribute)
import qualified Rx.Observable as Rx
import Types

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