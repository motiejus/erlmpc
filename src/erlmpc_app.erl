-module(erlmpc_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    io:format("Please open your WebSocket-aware browser: ~s~n",
        ["http://127.0.0.1:8080/"]),
    io:format("It would be useful if it supported HTML5 Range element.~n~n"
        "Google Chrome and safari are recommended.~n"),
    application:start(erlmpc).

start(_StartType, _StartArgs) ->
    erlmpc_sup:start_link().

stop(_State) ->
    ok.
