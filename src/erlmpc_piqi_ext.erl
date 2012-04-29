-module(erlmpc_piqi_ext).
-compile(export_all).


piqi() ->
    erlmpc_piqi:piqi().


parse_command(X, Format) ->
    erlmpc_piqi:parse_command(
        piqirun_ext:convert(?MODULE, <<"erlmpc/command">>, Format, 'pb', X)).



gen_command(X, Format) ->
    Iolist = erlmpc_piqi:gen_command(X),
    piqirun_ext:convert(?MODULE, <<"erlmpc/command">>, 'pb', Format, iolist_to_binary(Iolist)).





