%%% @author Motiejus Jakštys <desired.mta@gmail.com>
%%%
%%% @doc ErlMPC application starter
-module(erlmpc_starter).
-export([start_link/0]).

start_link() ->
    application:start(cowboy),
    application:start(piqi),
    application:start(gproc),
    Dispatch = [
        % {Host, list({Path, Handler, Opts})}
        {'_', [
                {[<<"static">>, '...'], cowboy_http_static, [
                        {directory, {priv_dir, erlmpc, [<<"static">>]}},
                        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                    ]},
                {'_', erlmpc_ws_handler, []}
            ]
        }
    ],

    cowboy:start_listener(erlmpc_ws_handler, 2,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).
