-module(erlmpc_piqi).
-compile(export_all).

-include_lib("piqi/include/piqirun.hrl").
-include("erlmpc_piqi.hrl").

-spec gen_int/2 :: (Code :: piqirun_code(), X :: integer()) -> iolist().
gen_int(Code, X) ->
    piqirun:integer_to_zigzag_varint(Code, X).


packed_gen_int(X) ->
    piqirun:integer_to_packed_zigzag_varint(X).

-spec gen_bool/2 :: (Code :: piqirun_code(), X :: boolean()) -> iolist().
gen_bool(Code, X) ->
    piqirun:boolean_to_varint(Code, X).


packed_gen_bool(X) ->
    piqirun:boolean_to_packed_varint(X).

-spec gen_float/2 :: (Code :: piqirun_code(), X :: number()) -> iolist().
gen_float(Code, X) ->
    piqirun:float_to_fixed64(Code, X).


packed_gen_float(X) ->
    piqirun:float_to_packed_fixed64(X).

-spec gen_binary/2 :: (Code :: piqirun_code(), X :: binary()) -> iolist().
gen_binary(Code, X) ->
    piqirun:binary_to_block(Code, X).

-spec gen_request/2 :: (Code :: piqirun_code(), X :: erlmpc_request()) -> iolist().
gen_request(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            {setvol, Y} -> gen_int(1, Y);
            next -> piqirun:gen_bool_field(2, true);
            prev -> piqirun:gen_bool_field(3, true);
            pause -> piqirun:gen_bool_field(4, true);
            status -> piqirun:gen_bool_field(5, true)
        end
    ).

-spec gen_status/2 :: (Code :: piqirun_code(), X :: erlmpc_status()) -> iolist().
gen_status(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(1, fun gen_int/2, X#erlmpc_status.volume),
        piqirun:gen_optional_field(2, fun gen_bool/2, X#erlmpc_status.repeat),
        piqirun:gen_optional_field(3, fun gen_bool/2, X#erlmpc_status.random),
        piqirun:gen_optional_field(4, fun gen_bool/2, X#erlmpc_status.single),
        piqirun:gen_optional_field(5, fun gen_bool/2, X#erlmpc_status.consume),
        piqirun:gen_optional_field(6, fun gen_int/2, X#erlmpc_status.playlist),
        piqirun:gen_optional_field(7, fun gen_int/2, X#erlmpc_status.playlistlength),
        piqirun:gen_optional_field(8, fun gen_state/2, X#erlmpc_status.state),
        piqirun:gen_optional_field(9, fun gen_int/2, X#erlmpc_status.song),
        piqirun:gen_optional_field(10, fun gen_int/2, X#erlmpc_status.songid),
        piqirun:gen_optional_field(11, fun gen_float/2, X#erlmpc_status.time),
        piqirun:gen_optional_field(12, fun gen_binary/2, X#erlmpc_status.elapsed),
        piqirun:gen_optional_field(13, fun gen_int/2, X#erlmpc_status.bitrate),
        piqirun:gen_optional_field(14, fun gen_int/2, X#erlmpc_status.xfade),
        piqirun:gen_optional_field(15, fun gen_binary/2, X#erlmpc_status.audio),
        piqirun:gen_optional_field(16, fun gen_int/2, X#erlmpc_status.updatings_db),
        piqirun:gen_optional_field(17, fun gen_binary/2, X#erlmpc_status.error)
    ]).

-spec gen_state/2 :: (Code :: piqirun_code(), X :: erlmpc_state()) -> iolist().
gen_state(Code, X) ->
    piqirun:integer_to_signed_varint(Code,
        case X of
            play -> 1;
        stop -> 2;
        pause -> 3
        end
    ).


packed_gen_state(X) ->
    piqirun:integer_to_packed_signed_varint(
        case X of
            play -> 1;
        stop -> 2;
        pause -> 3
        end
    ).

-spec gen_int/1 :: (X :: integer()) -> iolist().
gen_int(X) ->
    gen_int('undefined', X).

-spec gen_bool/1 :: (X :: boolean()) -> iolist().
gen_bool(X) ->
    gen_bool('undefined', X).

-spec gen_float/1 :: (X :: number()) -> iolist().
gen_float(X) ->
    gen_float('undefined', X).

-spec gen_binary/1 :: (X :: binary()) -> iolist().
gen_binary(X) ->
    gen_binary('undefined', X).

-spec gen_request/1 :: (X :: erlmpc_request()) -> iolist().
gen_request(X) ->
    gen_request('undefined', X).

-spec gen_status/1 :: (X :: erlmpc_status()) -> iolist().
gen_status(X) ->
    gen_status('undefined', X).

-spec gen_state/1 :: (X :: erlmpc_state()) -> iolist().
gen_state(X) ->
    gen_state('undefined', X).

-spec parse_int/1 :: (X :: piqirun_buffer()) -> integer().
parse_int(X) ->
    piqirun:integer_of_zigzag_varint(X).


packed_parse_int(X) ->
    piqirun:integer_of_packed_zigzag_varint(X).

-spec parse_bool/1 :: (X :: piqirun_buffer()) -> boolean().
parse_bool(X) ->
    piqirun:boolean_of_varint(X).


packed_parse_bool(X) ->
    piqirun:boolean_of_packed_varint(X).

-spec parse_float/1 :: (X :: piqirun_buffer()) -> float().
parse_float(X) ->
    piqirun:float_of_fixed64(X).


packed_parse_float(X) ->
    piqirun:float_of_packed_fixed64(X).

-spec parse_binary/1 :: (X :: piqirun_buffer()) -> binary().
parse_binary(X) ->
    piqirun:binary_of_block(X).

-spec parse_request/1 :: (X :: piqirun_buffer()) -> erlmpc_request().
parse_request(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        1 -> {setvol, parse_int(Obj)};
        2 when Obj == 1 -> next;
        3 when Obj == 1 -> prev;
        4 when Obj == 1 -> pause;
        5 when Obj == 1 -> status;
        _ -> piqirun:error_option(Obj, Code)
    end.

-spec parse_status/1 :: (X :: piqirun_buffer()) -> erlmpc_status().
parse_status(X) -> 
    R0 = piqirun:parse_record(X),
    {_Volume, R1} = piqirun:parse_optional_field(1, fun parse_int/1, R0),
    {_Repeat, R2} = piqirun:parse_optional_field(2, fun parse_bool/1, R1),
    {_Random, R3} = piqirun:parse_optional_field(3, fun parse_bool/1, R2),
    {_Single, R4} = piqirun:parse_optional_field(4, fun parse_bool/1, R3),
    {_Consume, R5} = piqirun:parse_optional_field(5, fun parse_bool/1, R4),
    {_Playlist, R6} = piqirun:parse_optional_field(6, fun parse_int/1, R5),
    {_Playlistlength, R7} = piqirun:parse_optional_field(7, fun parse_int/1, R6),
    {_State, R8} = piqirun:parse_optional_field(8, fun parse_state/1, R7),
    {_Song, R9} = piqirun:parse_optional_field(9, fun parse_int/1, R8),
    {_Songid, R10} = piqirun:parse_optional_field(10, fun parse_int/1, R9),
    {_Time, R11} = piqirun:parse_optional_field(11, fun parse_float/1, R10),
    {_Elapsed, R12} = piqirun:parse_optional_field(12, fun parse_binary/1, R11),
    {_Bitrate, R13} = piqirun:parse_optional_field(13, fun parse_int/1, R12),
    {_Xfade, R14} = piqirun:parse_optional_field(14, fun parse_int/1, R13),
    {_Audio, R15} = piqirun:parse_optional_field(15, fun parse_binary/1, R14),
    {_Updatings_db, R16} = piqirun:parse_optional_field(16, fun parse_int/1, R15),
    {_Error, R17} = piqirun:parse_optional_field(17, fun parse_binary/1, R16),
    piqirun:check_unparsed_fields(R17),
    #erlmpc_status{
        volume = _Volume,
        repeat = _Repeat,
        random = _Random,
        single = _Single,
        consume = _Consume,
        playlist = _Playlist,
        playlistlength = _Playlistlength,
        state = _State,
        song = _Song,
        songid = _Songid,
        time = _Time,
        elapsed = _Elapsed,
        bitrate = _Bitrate,
        xfade = _Xfade,
        audio = _Audio,
        updatings_db = _Updatings_db,
        error = _Error
    }.

-spec parse_state/1 :: (X :: piqirun_buffer()) -> erlmpc_state().
parse_state(X) ->
    case piqirun:integer_of_signed_varint(X) of
        1 -> play;
        2 -> stop;
        3 -> pause;
        Y -> piqirun:error_enum_const(Y)
    end.


packed_parse_state(X) ->
    {Code, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {case Code of
        1 -> play;
        2 -> stop;
        3 -> pause;
        Y -> piqirun:error_enum_const(Y)
    end, Rest}.


piqi() ->
    [
        <<226,202,230,52,6,101,114,108,109,112,99,160,148,209,72,129,248,174,104,234,134,149,130,4,119,170,136,200,184,14,113,218,164,238,191,4,7,114,101,113,117,101,115,116,170,183,218,222,5,27,218,164,238,191,4,6,115,101,116,118,111,108,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,170,183,218,222,5,10,218,164,238,191,4,4,110,101,120,116,170,183,218,222,5,10,218,164,238,191,4,4,112,114,101,118,170,183,218,222,5,11,218,164,238,191,4,5,112,97,117,115,101,170,183,218,222,5,12,218,164,238,191,4,6,115,116,97,116,117,115,234,134,149,130,4,155,6,138,233,142,251,14,148,6,210,203,242,36,39,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,6,118,111,108,117,109,101,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,40,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,6,114,101,112,101,97,116,210,171,158,194,6,10,218,164,238,191,4,4,98,111,111,108,210,203,242,36,40,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,6,114,97,110,100,111,109,210,171,158,194,6,10,218,164,238,191,4,4,98,111,111,108,210,203,242,36,40,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,6,115,105,110,103,108,101,210,171,158,194,6,10,218,164,238,191,4,4,98,111,111,108,210,203,242,36,41,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,7,99,111,110,115,117,109,101,210,171,158,194,6,10,218,164,238,191,4,4,98,111,111,108,210,203,242,36,41,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,8,112,108,97,121,108,105,115,116,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,47,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,14,112,108,97,121,108,105,115,116,108,101,110,103,116,104,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,40,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,5,115,116,97,116,101,210,171,158,194,6,11,218,164,238,191,4,5,115,116,97,116,101,210,203,242,36,37,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,4,115,111,110,103,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,39,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,6,115,111,110,103,105,100,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,39,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,4,116,105,109,101,210,171,158,194,6,11,218,164,238,191,4,5,102,108,111,97,116,210,203,242,36,43,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,7,101,108,97,112,115,101,100,210,171,158,194,6,12,218,164,238,191,4,6,98,105,110,97,114,121,210,203,242,36,40,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,7,98,105,116,114,97,116,101,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,38,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,5,120,102,97,100,101,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,41,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,5,97,117,100,105,111,210,171,158,194,6,12,218,164,238,191,4,6,98,105,110,97,114,121,210,203,242,36,45,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,12,117,112,100,97,116,105,110,103,115,45,100,98,210,171,158,194,6,9,218,164,238,191,4,3,105,110,116,210,203,242,36,41,154,182,154,152,4,6,128,250,213,155,15,1,218,164,238,191,4,5,101,114,114,111,114,210,171,158,194,6,12,218,164,238,191,4,6,98,105,110,97,114,121,218,164,238,191,4,6,115,116,97,116,117,115,234,134,149,130,4,66,138,176,205,197,1,60,218,164,238,191,4,5,115,116,97,116,101,170,183,218,222,5,10,218,164,238,191,4,4,112,108,97,121,170,183,218,222,5,10,218,164,238,191,4,4,115,116,111,112,170,183,218,222,5,11,218,164,238,191,4,5,112,97,117,115,101>>
    ].

