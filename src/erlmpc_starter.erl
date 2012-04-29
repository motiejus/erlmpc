-module(erlmpc_starter).
-export([start_link/0]).

start_link() ->
    application:start(cowboy),
    application:start(piqi),
    Dispatch = [
        % {Host, list({Path, Handler, Opts})}
        {'_', [
                {[<<"static">>, '...'], cowboy_http_static,
                    [{directory, {priv_dir, erlmpc, [<<"static">>]}}]},
                {'_', erlmpc_ws_handler, []}
            ]
        }
    ],

    cowboy:start_listener(erlmpc_ws_handler, 2,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).
