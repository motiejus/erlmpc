-module(erlmpc_starter).
-export([start_link/0]).

start_link() ->
    application:start(cowboy),

    Dispatch = [
        % {Host, list({Path, Handler, Opts})}
        {'_', [{[<<"static">>, '...'], erlmpc_static_handler, []}]},
        {'_', [{'_', erlmpc_ws_handler, []}]}
    ],

    cowboy:start_listener(erlmpc_static_handler, 2,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    cowboy:start_listener(erlmpc_ws_handler, 2,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).
