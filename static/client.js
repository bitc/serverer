(function() {

    function createWebSocket(path) {
        var uri = 'ws://' + window.location.host + path;

        var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
        return new Socket(uri);
    }

    var ws = createWebSocket('/');

    ws.onopen = function() {
        alert('open');
    };

    ws.onmessage = function(event) {
        console.log(event);
        alert('message: ' + event.data);
    };

})();
