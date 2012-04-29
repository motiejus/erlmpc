-ifndef(__ERLMPC_PIQI_HRL__).
-define(__ERLMPC_PIQI_HRL__, 1).

-type(erlmpc_request() :: 
      {setvol, integer()}
    | next
    | prev
    | pause
    | status
).
-type(erlmpc_state() :: 
      play
    | stop
    | pause
).
-record(erlmpc_status, {
    volume :: integer(),
    repeat :: boolean(),
    random :: boolean(),
    single :: boolean(),
    consume :: boolean(),
    playlist :: integer(),
    playlistlength :: integer(),
    state :: boolean(),
    song :: integer(),
    songid :: integer(),
    time :: number(),
    elapsed :: binary(),
    bitrate :: integer(),
    xfade :: integer(),
    audio :: binary(),
    updatings_db :: integer(),
    error :: binary()
}).

-type(erlmpc_status() :: #erlmpc_status{}).


-endif.
