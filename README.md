erlmpc
======

Erlang web-based MPD client

Uses these buzzwords:

* PIQI. The most awesomest thing after Proper.
* Websockets. Actions in browser are really visible immediatly.
* Cowboy for wrapping it all up ;)

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
