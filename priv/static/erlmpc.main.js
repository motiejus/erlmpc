ErlMPC = new Object();

ErlMPC.init = function(x) {
    if ("WebSocket" in window) {
        ErlMPC.cache = {
            "seek" : $("#seek"),
            "toggle" : $("#toggle"),
            "setvol" : $("#setvol"),
            "songname" : $("#songname")
        },

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

ErlMPC.update_screen = function(data) {
    ErlMPC.cache.songname.html(data.currentsong.Artist + " - " + data.currentsong.Title);
    ErlMPC.cache.toggle.attr("value", data.state == "play"? "Pause" : "Play" );
    ErlMPC.cache.setvol.attr("value", data.volume);
    ErlMPC.cache.seek.attr({"max" : Math.round(data.currentsong.Time),
            "value" : Math.round(data.time) });
    if (data.state == "play")
        ErlMPC.timeout_handle = setTimeout(ErlMPC.run_freq, ErlMPC.timeout);
    else
        ErlMPC.stop_freq();
};

ErlMPC.onopen = function(evt) {
    // Ask for current song, volume and status
    ErlMPC.ws.send("{ \"statuscurrentsong\": null }");
};

ErlMPC.onclose = function(evt) {
    ErlMPC.stop_freq();
    alert("Disconnected");
};

ErlMPC.onmessage = function(evt) {
    // We accept only one kind of message: json statuscurrentsong
    var json = JSON.parse(evt.data);
    ErlMPC.update_screen(json);
};

ErlMPC.onerror = function(evt) {
    console.log(evt.data);
};

ErlMPC.timeout = 1000;
ErlMPC.timeout_handle = undefined;
ErlMPC.run_freq = function() {
    ErlMPC.stop_freq();
    ErlMPC.cache.seek.attr("value",
            parseInt(ErlMPC.cache.seek.attr("value")) + ErlMPC.timeout/1000);
    ErlMPC.timeout_handle = setTimeout(ErlMPC.run_freq, ErlMPC.timeout);
};
ErlMPC.stop_freq = function() {
    if (ErlMPC.timeout_handle != undefined)
        clearTimeout(ErlMPC.timeout_handle);
    ErlMPC.timeout_handle = undefined;
};

$(function() {
        ErlMPC.init();
});
