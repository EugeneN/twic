// module Utils


function setTitle(a) {
    return function() {
        document.title = a;
        return undefined;
    }
}


function toString(a){
console.log('toString:', a);
return a.toString();
}


function readInt(a){
console.log('readInt:', a);
var z = parseInt(a, 10);
return z;
}

exports.fromEvent = function (ev) { return function() {return Rx.Observable.fromEvent(document.body, ev)} }

function isNumeric(a){
return /^[0-9]+$/.test(a)
}



function scrollToTop() {
console.log('scroll to top');
window.scroll(0,0);
return undefined;
}


function jsonStringify(o) {
return JSON.stringify(o);
}


function extractTarget(ev){
return ev.target;
}


function extractCoords(ev){
return [ev.clientX, ev.clientY];
}



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


oneSecond = 1000
oneMinute = 60 * oneSecond



function getIntervalStream(n) {
    return Rx.Observable.interval(n);
}


function filterRx(f){
    return function(observable) {
        return observable.filter(f)
    }
}




function byId(idstr){
    return function(event) {
        return event.target.id === idstr;
    }
}


function value(el) {
    return el.value;
}






function scrollToEl(id){
    console.log("scroll to", id, document.getElementById(id));
    document.getElementById(id).scrollIntoView(true) }






function forkPostpone(f) {
    return function(delay) {
        return function() {
            console.log("postponing ", f, delay)
            setTimeout(f, delay);
        }
    }
}




function splitAt(as) {
    return function(idx) {
        return [as.slice(0, idx), as.slice(idx)]
    }
}





function getDeltaY(e) { return e.originalEvent.deltaY }




function bufferWithTime(time) {
    return function(obs) {
        return obs.bufferWithTime(time)
    }
}




function throttleWithTimeout(time) {
    return function(obs) {
        return obs.throttleWithTimeout(time)
    }
}

function setFocus(id) { return function() { jQuery('#' + id).focus(); }}
function which(ev) { return ev.which; }


function getWheelObservable(x) {
    return Rx.Observable.fromEvent(document, 'wheel')
}




function getNewObservable(x) {
    return new Rx.Subject()
}




function publishToObservable(obs){
    return function (val) {
        obs.onNext(val)
    }
}




function setProps(view) {
    return function(props) {
        return function(){
            view.setProps(props)
            return null
        }
    }
}


function showuuid(ident) {
    return ident.toString();
}



function runUUID(UUID) {
    return UUID();
}



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




function callEventHandler(f){
    return function(e){ return f(e)() }
}




function stopPropagation(e) { return function() {e.stopPropagation(); e.preventDefault(); } }




function stringReplace(src){
    return function(pattern) {
        return function(replacement){
            return src.replace(pattern, replacement)
        }
    }
}
