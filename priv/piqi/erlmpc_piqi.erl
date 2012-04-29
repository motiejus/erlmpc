-module(erlmpc_piqi).
-compile(export_all).

-include_lib("piqi/include/piqirun.hrl").
-include("erlmpc_piqi.hrl").

-spec gen_int/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().
gen_int(Code, X) ->
    piqirun:integer_to_zigzag_varint(Code, X).


packed_gen_int(X) ->
    piqirun:integer_to_packed_zigzag_varint(X).

-spec gen_command/2 :: (Code :: piqirun_code(), X :: erlmpc_command()) -> iolist().
gen_command(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            {vol_abs, Y} -> gen_int(1, Y);
            {vol_rel, Y} -> gen_int(2, Y)
        end
    ).

-spec gen_int/1 :: (X :: integer()) -> iolist().
gen_int(X) ->
    gen_int('undefined', X).

-spec gen_command/1 :: (X :: erlmpc_command()) -> iolist().
gen_command(X) ->
    gen_command('undefined', X).

-spec parse_int/1 :: (X :: piqirun_buffer()) -> integer().
parse_int(X) ->
    piqirun:integer_of_zigzag_varint(X).


packed_parse_int(X) ->
    piqirun:integer_of_packed_zigzag_varint(X).

-spec parse_command/1 :: (X :: piqirun_buffer()) -> erlmpc_command().
parse_command(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        1 -> {vol_abs, parse_int(Obj)};
        2 -> {vol_rel, parse_int(Obj)};
        _ -> piqirun:error_option(Obj, Code)
    end.


piqi() ->
    [
        <<226,202,230,52,6,101,114,108,109,112,99,160,148,209,72,129,248,174,104,234,134,149,130,4,87,170,136,200,184,14,81,218,164,238,191,4,7,99,111,109,109,97,110,100,170,183,218,222,5,28,218,164,238,191,4,7,118,111,108,45,97,98,115,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,170,183,218,222,5,28,218,164,238,191,4,7,118,111,108,45,114,101,108,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116>>
    ].

