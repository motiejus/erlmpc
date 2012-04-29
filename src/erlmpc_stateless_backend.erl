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
        status ->
            St = erlmpd:status(Conn),
            Ret = list_to_tuple([erlmpc_status|[proplists:get_value(X,St) ||
                        X <- record_info(fields, erlmpc_status)]]),
            BinRet = erlmpc_piqi_ext:gen_status(Ret, 'json_pretty'),
            {reply, BinRet}
    end.
