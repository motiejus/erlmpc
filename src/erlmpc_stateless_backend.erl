%%% @author Motiejus Jakštys <desired.mta@gmail.com>
%%%
%%% @doc ErlMPC backend which decodes requests and speaks to erlmpd backend

-module(erlmpc_stateless_backend).
-include("priv/piqi/erlmpc_piqi.hrl").

-export([proc/2]).

%% @doc process a request from client:
%% 1) Depiqi it (convert to native type)
%% 2) Find out what it wants
%% 3) Do the action
%% 4) If necessary, give response
-spec proc/2 :: (binary(), erlmpd:conn()) -> {reply, binary()} | noreply.
proc(Msg, Conn) ->
    Req = erlmpc_piqi_ext:parse_request(Msg, 'json'),
    case Req of
        currentsong ->
            R = get_currentsong(Conn),
            {reply, erlmpc_piqi_ext:gen_currentsong(R, 'json_pretty')};
        status ->
            St = get_status(Conn),
            {reply, erlmpc_piqi_ext:gen_status(St, 'json_pretty')};
        statuscurrentsong -> % status and current song
            St = get_status(Conn),
            R = St#erlmpc_status{currentsong=get_currentsong(Conn)},
            {reply, erlmpc_piqi_ext:gen_status(R, 'json_pretty')}
    end.

get_status(Conn) ->
    St = erlmpd:status(Conn),
    list_to_tuple([erlmpc_status|[proplists:get_value(X, St) ||
                X <- record_info(fields, erlmpc_status)]]).

get_currentsong(Conn) ->
    CurrentSong = erlmpd:currentsong(Conn),
    #erlmpc_currentsong{
        file = proplists:get_value(file, CurrentSong),
        time = proplists:get_value('Time', CurrentSong),
        title = proplists:get_value('Title', CurrentSong),
        album = proplists:get_value('Album', CurrentSong),
        track = proplists:get_value('Track', CurrentSong),
        genre = proplists:get_value('Genre', CurrentSong),
        pos = proplists:get_value('Pos', CurrentSong),
        id = proplists:get_value('Id', CurrentSong)
    }.
