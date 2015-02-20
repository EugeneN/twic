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
    """function toString(a){                
        console.log('toString:', a);     
        return a.toString();             
    } """ :: forall a. a -> String

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
    """function scrollToTop() {             
           console.log('scroll to top'); 
           window.scroll(0,0);           
           return undefined;             
    }""" :: forall eff. (Eff (dom :: DOM | eff) Unit)

foreign import jsonStringify
    """function jsonStringify(o) {
      return JSON.stringify(o);
    }""" :: forall r. { | r} -> String

foreign import extractTarget
    """function extractTarget(ev){
        return ev.target;
    } """ :: J.JQueryEvent -> HTMLElement

foreign import extractCoords
    """function extractCoords(ev){
        return [ev.clientX, ev.clientY];
    } """ :: J.JQueryEvent -> [Number]

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
    """
    function value(el) {
      return el.value;
    }""" :: Element -> String

foreign import setFocus "function setFocus(id) { return function() { jQuery('#' + id).focus(); }}" :: forall eff. String -> Eff eff Unit
foreign import which "function which(ev) { return ev.which; }" :: forall a. a -> Number

foreign import scrollToEl
    """
    function scrollToEl(id){
        console.log("scroll to", id, document.getElementById(id));
        document.getElementById(id).scrollIntoView(true) }
    """ :: forall eff. String -> (Eff (dom :: DOM | eff) Unit)

foreign import data Timeout :: !

foreign import forkPostpone
    """
    function forkPostpone(f) {
        return function(delay) {
            return function() {
                console.log("postponing ", f, delay)
                setTimeout(f, delay);
            }
        }
    }
    """ :: forall a eff. a -> Number -> Eff (|eff) Unit

foreign import splitAt
    """
    function splitAt(as) {
        return function(idx) {
            return [as.slice(0, idx), as.slice(idx)]
        }
    }
    """ :: forall a. [a] -> Number -> [[a]]


foreign import getDeltaY
    """
    function getDeltaY(e) { return e.originalEvent.deltaY }
    """ :: forall a. a -> Number

foreign import bufferWithTime
    """
    function bufferWithTime(time) {
        return function(obs) {
            return obs.bufferWithTime(time)
        }
    }
    """ :: forall a. Number -> Rx.Observable a -> Rx.Observable [a]

foreign import throttleWithTimeout
    """
    function throttleWithTimeout(time) {
        return function(obs) {
            return obs.throttleWithTimeout(time)
        }
    }
    """ :: forall a. Number -> Rx.Observable a -> Rx.Observable a

foreign import getWheelObservable
  """
  function getWheelObservable(x) {
    return Rx.Observable.fromEvent(document, 'wheel')
  }
  """ :: forall a b. b -> Rx.Observable a

foreign import getNewObservable
    """
    function getNewObservable(x) {
        return new Rx.Subject()
    }
    """ :: forall a b. a -> Rx.Observable b

foreign import publishToObservable
    """
    function publishToObservable(obs){
        return function (val) {
                obs.onNext(val)
        }
    }
    """ :: forall a b. Rx.Observable a -> b -> Rx.Observable a

foreign import setProps
    """
    function setProps(view) {
        return function(props) {
            return function(){
                view.setProps(props)
                return null
            }
        }
    }
    """ :: forall a eff. a -> RefVal State ->  Eff (dom :: DOM, react :: React | eff) Unit

data KeyCode = Insert
             | Escape
             | Enter
             | Delete
             | F2
             | F4
             | F5
             | UnknownKey Number

keyEventToKeyCode :: forall a. a -> KeyCode -- JQueryEvent a, ReactEvent a =>
keyEventToKeyCode x | which x == 13  = Enter
                    | which x == 27  = Escape
                    | which x == 45  = Insert
                    | which x == 46  = Delete
                    | which x == 113 = F2
                    | which x == 115 = F4
                    | which x == 116 = F5

keyEventToKeyCode x                  = UnknownKey $ which x

instance eqKeyCode :: Eq KeyCode where
    (==) Insert Insert = true
    (==) Escape Escape = true
    (==) Enter  Enter  = true
    (==) Delete Delete = true
    (==) F2     F2     = true
    (==) F4     F4     = true
    (==) F5     F5     = true
    (==) _      _      = false

    (/=) a      b      = not $ (==) a b



instance eqUUID :: Eq UUID where
    (==) ident ident' = showuuid ident == showuuid ident'
    (/=) ident ident' = not (ident == ident')

instance showUUID :: Show UUID where
    show ident = showuuid ident

foreign import showuuid
    """
    function showuuid(ident) {
      return ident.toString();
    }""" :: UUID -> String

foreign import runUUID
    """
    function runUUID(UUID) {
      return UUID();
    }""" :: Eff (uuid :: UUIDEff) UUID -> UUID

foreign import getUUID
    """
    function getUUID() {
      var i, itoh, s, t, _i;
      s = [];
      itoh = '0123456789ABCDEF'.split('');
      s = (function() {
        var _i, _results;
        _results = [];
        for (i = _i = 0; _i <= 35; i = ++_i) {
          _results.push(Math.floor(Math.random() * 0x10));
        }
        return _results;
      })();
      t = (new Date()).getTime() & 0x7FFFFFFF;
      for (i = _i = 0; _i <= 3; i = ++_i) {
        s[i] = t & 0xF;
        t >>= 8;
      }
      s[14] = 4;
      s[19] = (s[19] & 0x3) | 0x8;
      s = (function() {
        var _j, _len, _results;
        _results = [];
        for (_j = 0, _len = s.length; _j < _len; _j++) {
          i = s[_j];
          _results.push(itoh[s[i]]);
        }
        return _results;
      })();
      s[8] = s[13] = s[18] = s[23] = '-';
      return s.join('');
    };
    """ :: forall eff. Eff (uuid :: UUIDEff | eff) UUID