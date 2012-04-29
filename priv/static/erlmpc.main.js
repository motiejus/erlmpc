var ErlMPC = ErlMPC || Object;

ErlMPC.onopen = function(evt) {
    // Ask for current song, volume and status
    ErlMPC.ws.send("{ \"get_status\": null }");
};

ErlMPC.onclose = function(evt) {
    alert("Disconnected");
};

ErlMPC.onmessage = function(evt) {
    console.log(evt.data);
};

ErlMPC.onerror = function(evt) {
    console.log(evt.data);
};

ErlMPC.init = function(x) {
    if ("WebSocket" in window) {
        wsUri = document.URL.replace(/^http:/, "ws:");
        ErlMPC.ws = new WebSocket(wsUri);
        ErlMPC.ws.onopen = ErlMPC.onopen;
        ErlMPC.ws.onclose = ErlMPC.onclose;
        ErlMPC.ws.onmessage = ErlMPC.onmessage;
        ErlMPC.ws.onerror = ErlMPC.onerror;
    } else {
        alert("Your browser does not support websockets, sorry");
        return false;
    }
};

(function($) {
    ErlMPC.init();
})(jQuery);
