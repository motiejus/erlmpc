%%% @author Motiejus Jakštys <desired.mta@gmail.com>
%%%
%%% @doc ErlMPC backend which decodes requests and speaks to erlmpd backend

-module(erlmpc_stateless_backend).
-include("priv/piqi/erlmpc_piqi.hrl").

-export([proc/2, proc_bin/2]).
-export([get_currentsong/1, get_status/1]).

%% @doc process a request when action is known
%% 1) Find out what it wants
%% 2) Do the action
%% 3) If necessary, give response
-spec proc/2 :: (atom(), erlmpd:mpd_conn()) -> {reply, binary()} | noreply.
proc(Req, Conn) ->
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
            {reply, erlmpc_piqi_ext:gen_status(R, 'json_pretty')};
        {seek, SeekSecs} ->
            SongId = proplists:get_value(songid, erlmpd:status(Conn)),
            erlmpd:seekid(Conn, SongId, SeekSecs),
            noreply;
        {setvol, Vol} -> erlmpd:setvol(Conn, Vol), noreply;
        {pause, true} -> erlmpd:pause(Conn, true), noreply;
        {pause, false} -> erlmpd:play(Conn), noreply;
        prev -> erlmpd:previous(Conn), noreply;
        next -> erlmpd:next(Conn), noreply
    end.

%% @doc process a request from client:
%% 1) Depiqi it (convert to native type)
%% 2) Pass to request processor
-spec proc_bin/2 :: (binary(), erlmpd:mpd_conn()) -> {reply, binary()} | noreply.
proc_bin(Msg, Conn) ->
    Req = erlmpc_piqi_ext:parse_request(Msg, 'json'),
    proc(Req, Conn).

get_status(Conn) ->
    St = erlmpd:status(Conn),
    list_to_tuple([erlmpc_status|[proplists:get_value(X, St) ||
                X <- record_info(fields, erlmpc_status)]]).

get_currentsong(Conn) ->
    CurrentSong = erlmpd:currentsong(Conn),
    % Track is sometimes int, sometimes string from MPD. Converting to string
    Track = case proplists:get_value('Track', CurrentSong) of
        T when is_integer(T) -> integer_to_list(T);
        T -> T
    end,
    #erlmpc_currentsong{
        file = proplists:get_value(file, CurrentSong),
        time = proplists:get_value('Time', CurrentSong),
        artist = proplists:get_value('Artist', CurrentSong),
        title = proplists:get_value('Title', CurrentSong),
        album = proplists:get_value('Album', CurrentSong),
        track = Track,
        genre = proplists:get_value('Genre', CurrentSong),
        pos = proplists:get_value('Pos', CurrentSong),
        id = proplists:get_value('Id', CurrentSong)
    }.
