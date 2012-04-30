erlmpc
======

Erlang web-based MPD client.

Major highlights (a.k.a. buzzwords):

* PIQI. The awesomest thing after Proper. Look at my separation of concerns :)
* Websockets. Actions are really visible immediately in browser.

How to use:

    $ git clone git://github.com/Motiejus/erlmpc.git
    $ cd erlmpc
    $ make
    $ make run

Open http://127.0.0.1:8080/ in a WebSocket-capable browser. Google Chrome 14+
is recommended.

TODO:

    * Get rid of gproc (too many dependencies, too less used)
    * Blog about PIQI
    * (Maybe) Add long polling fallback if websockets are unavailable

Read the whole story of this player [here].

[here]: http://m.jakstys.lt/tech/2012/04/erlmpc-and-the-awesomeness-of-piqi/
