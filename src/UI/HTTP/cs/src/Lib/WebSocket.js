// inherited from https://github.com/hdgarrood/multipac/blob/master/src/BrowserWebSocket.purs

//    module Lib.WebSocket

function mkWebSocket(url) {
    return function() {
        return new WebSocket(url)
    }
}

function onMessageImpl(socket, callback) {
    return function() {
        socket.onmessage = function(msg) {
            callback(msg.data)()
        }
    }
}

function onErrorImpl(socket, callback) {
    return function() {
        socket.onerror = callback
    }
}

function onCloseImpl(socket, callback) {
    return function() {
        socket.onclose = callback
    }
}

function sendImpl(socket, message) {
    return function() {
        socket.send(message)
    }
}