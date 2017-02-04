(function() {
    function createWebSocket(path) {
        var uri = 'ws://' + window.location.host + path;
        var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
        return new Socket(uri);
    }

    function reloadCSS() {
        var linkTags = document.getElementsByTagName("link");
        for (var i = 0; i < linkTags.length; ++i) {
            var link = linkTags[i];
            if (link.getAttribute("rel") == "stylesheet") {
                console.log("Reloading " + link.getAttribute("href"));
                link.setAttribute("href", link.getAttribute("href"));
            }
        }
    }

    var ws = createWebSocket('/__listen');

    ws.onmessage = function(e) {
        if (e.data === 'reload') {
            location.reload(true);
        } else if (e.data === 'reload_css') {
            console.log("Received reload CSS request");
            reloadCSS();
        } else {
            console.warn("Received unknown event from serverer:", e.data);
        }
    };
})();
