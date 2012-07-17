ErlMPC = new Object();

ErlMPC.init = function( serverUrl ) {
    if ("WebSocket" in window) {
        ErlMPC.cache = {
            "seek" : $("#seek"),
            "pause" : $("#pause"),
            "setvol" : $("#setvol"),
            "songname" : $("#songname"),
            "next" : $("#next"),
            "prev" : $("#prev"),
            "error" : $("#error")
        };
        ErlMPC.h = {};

        ErlMPC.h.wsUri = serverUrl;
        ErlMPC.ws = new WebSocket(ErlMPC.h.wsUri);
        ErlMPC.ws.onopen = ErlMPC.onopen;
        ErlMPC.ws.onclose = ErlMPC.onclose;
        ErlMPC.ws.onmessage = ErlMPC.onmessage;
        ErlMPC.ws.onerror = ErlMPC.onerror;

        ErlMPC.h.ChangeAfterSomeTime = function(obj, name) {
            obj.change(function() {
                if (obj.data('t') != undefined)
                    clearTimeout(obj.data('t'));
                var Val = "{ \"" + name + "\" : " + this.value + "}";
                obj.data("t", setTimeout(function(){ErlMPC.ws.send(Val)},50));
                })
        },
        ErlMPC.h.ChangeAfterSomeTime(ErlMPC.cache.seek, "seek");
        ErlMPC.h.ChangeAfterSomeTime(ErlMPC.cache.setvol, "setvol");
        ErlMPC.cache.next.click(function() { ErlMPC.ws.send("{ \"next\": true }"); });
        ErlMPC.cache.prev.click(function() { ErlMPC.ws.send("{ \"prev\": true }"); });
        ErlMPC.cache.pause.click(function() {
            var Val = { "pause" : !ErlMPC.cache.pause.data('paused') };
            ErlMPC.ws.send(JSON.stringify(Val));
        });
    } else {
        alert("Your browser does not support websockets, sorry");
        return false;
    }
};

ErlMPC.update_screen = function(data) {
    ErlMPC.cache.error.html(data['error'] ? data['error'] : "");
    ErlMPC.cache.songname.html(data.currentsong.Artist + " - " + data.currentsong.Title);
    var paused = data.state != "play";
    ErlMPC.cache.pause.attr("value", paused? "Play" : "Pause" ).data('paused', paused);
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
    ErlMPC.ws.send("{ \"statuscurrentsong\": true }");
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
	var iurl = document.location,
	    url;

    if (iurl.hash) {
    	url = "ws:/" + iurl.hash.substr(1);
    } else {
    	url = "ws:/" + iurl.href;
    }
    ErlMPC.init(url);
});
