-module(erlmpc_ws_handler).

-behaviour(cowboy_http_websocket_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.


handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200,
        [{'Content-Type', <<"text/html">>}],
        <<"TBD: index.html or something">>,
        Req
    ),
    {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.
websocket_terminate(_Reason, _Req, _State) ->
    ok.
