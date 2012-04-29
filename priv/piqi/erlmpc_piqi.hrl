-ifndef(__ERLMPC_PIQI_HRL__).
-define(__ERLMPC_PIQI_HRL__, 1).

-type(erlmpc_request() :: 
      {setvol, integer()}
    | {seek, integer()}
    | next
    | prev
    | pause
    | status
    | currentsong
    | statuscurrentsong
).
-record(erlmpc_currentsong, {
    file :: string() | binary(),
    time :: integer(),
    artist :: string() | binary(),
    title :: string() | binary(),
    album :: string() | binary(),
    track :: string() | binary(),
    date :: string() | binary(),
    genre :: string() | binary(),
    pos :: integer(),
    id :: integer()
}).
-record(erlmpc_status, {
    volume :: integer(),
    repeat :: boolean(),
    random :: boolean(),
    single :: boolean(),
    consume :: boolean(),
    playlist :: integer(),
    playlistlength :: integer(),
    state :: erlmpc_state(),
    song :: integer(),
    songid :: integer(),
    time :: number(),
    elapsed :: string() | binary(),
    bitrate :: integer(),
    xfade :: integer(),
    audio :: string() | binary(),
    updatings_db :: integer(),
    error :: string() | binary(),
    currentsong :: erlmpc_currentsong()
}).
-type(erlmpc_state() :: 
      play
    | stop
    | pause
).

-type(erlmpc_currentsong() :: #erlmpc_currentsong{}).
-type(erlmpc_status() :: #erlmpc_status{}).


-endif.
