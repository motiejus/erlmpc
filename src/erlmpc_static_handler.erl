-module(erlmpc_static_handler).
-export([init/3]).
-export([handle/2, terminate/2]).

init({tcp, http}, Req, Opts) ->
        {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
