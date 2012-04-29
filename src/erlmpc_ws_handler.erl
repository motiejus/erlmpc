%%% @author Motiejus Jakštys <desired.mta@gmail.com>
%%%
%%% @doc ErlMPC websocket handler

-module(erlmpc_ws_handler).

-behaviour(cowboy_http_websocket_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
        websocket_info/3, websocket_terminate/3]).

-record(state, {
        conn :: erlmpd:conn()
    }).

init({_Any, http}, Req, []) ->
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.


handle(Req, State) ->
    {ok, IndexHtml} = file:read_file([code:priv_dir(erlmpc), "/static/index.html"]),
    Headers = [{'Content-Type', <<"text/html">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, IndexHtml, Req),
    {ok, Req2, State}.

websocket_init(_TransportName, Req, _Opts) ->
    %erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, Conn} = erlmpd:connect(),
    {ok, Req, #state{conn=Conn}}.

websocket_handle({text, Msg}, Req, State=#state{conn=Conn}) ->
    case erlmpc_stateless_backend:proc(Msg, Conn) of
        {reply, Reply} ->
            {reply, {text, Reply}, Req, State};
        noreply ->
            {ok, Req, State}
    end.

    %{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
%websocket_handle({text, Msg}, Req, State) ->
    %{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
%websocket_handle(_Data, Req, State) ->
%    {ok, Req, State}.

%websocket_info({timeout, _Ref, Msg}, Req, State) ->
%    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
%    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    io:format("Something received: ~p~n", [_Info]),
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.
websocket_terminate(_Reason, _Req, _State) ->
    ok.
