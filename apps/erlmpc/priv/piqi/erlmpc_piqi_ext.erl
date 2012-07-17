-module(erlmpc_piqi_ext).
-compile(export_all).


piqi() ->
    erlmpc_piqi:piqi().


parse_request(X, Format) ->
    erlmpc_piqi:parse_request(
        piqirun_ext:convert(?MODULE, <<"erlmpc/request">>, Format, 'pb', X)).

parse_request(X, Format, Options) ->
    erlmpc_piqi:parse_request(
        piqirun_ext:convert(?MODULE, <<"erlmpc/request">>, Format, 'pb', X, Options)).

parse_currentsong(X, Format) ->
    erlmpc_piqi:parse_currentsong(
        piqirun_ext:convert(?MODULE, <<"erlmpc/currentsong">>, Format, 'pb', X)).

parse_currentsong(X, Format, Options) ->
    erlmpc_piqi:parse_currentsong(
        piqirun_ext:convert(?MODULE, <<"erlmpc/currentsong">>, Format, 'pb', X, Options)).

parse_status(X, Format) ->
    erlmpc_piqi:parse_status(
        piqirun_ext:convert(?MODULE, <<"erlmpc/status">>, Format, 'pb', X)).

parse_status(X, Format, Options) ->
    erlmpc_piqi:parse_status(
        piqirun_ext:convert(?MODULE, <<"erlmpc/status">>, Format, 'pb', X, Options)).

parse_state(X, Format) ->
    erlmpc_piqi:parse_state(
        piqirun_ext:convert(?MODULE, <<"erlmpc/state">>, Format, 'pb', X)).

parse_state(X, Format, Options) ->
    erlmpc_piqi:parse_state(
        piqirun_ext:convert(?MODULE, <<"erlmpc/state">>, Format, 'pb', X, Options)).



gen_request(X, Format) ->
    Iolist = erlmpc_piqi:gen_request(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/request">>, 'pb', Format, iolist_to_binary(Iolist)).

gen_request(X, Format, Options) ->
    Iolist = erlmpc_piqi:gen_request(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/request">>, 'pb', Format, iolist_to_binary(Iolist), Options).

gen_currentsong(X, Format) ->
    Iolist = erlmpc_piqi:gen_currentsong(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/currentsong">>, 'pb', Format, iolist_to_binary(Iolist)).

gen_currentsong(X, Format, Options) ->
    Iolist = erlmpc_piqi:gen_currentsong(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/currentsong">>, 'pb', Format, iolist_to_binary(Iolist), Options).

gen_status(X, Format) ->
    Iolist = erlmpc_piqi:gen_status(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/status">>, 'pb', Format, iolist_to_binary(Iolist)).

gen_status(X, Format, Options) ->
    Iolist = erlmpc_piqi:gen_status(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/status">>, 'pb', Format, iolist_to_binary(Iolist), Options).

gen_state(X, Format) ->
    Iolist = erlmpc_piqi:gen_state(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/state">>, 'pb', Format, iolist_to_binary(Iolist)).

gen_state(X, Format, Options) ->
    Iolist = erlmpc_piqi:gen_state(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/state">>, 'pb', Format, iolist_to_binary(Iolist), Options).





