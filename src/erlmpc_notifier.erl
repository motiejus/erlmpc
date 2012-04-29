%%% @author Motiejus Jakštys <desired.mta@gmail.com>
%%%
%%% @doc ErlMPC client notifier. Broadcasts erlmpd events.

-module(erlmpc_notifier).

-export([start_link/0, wait_for_events/1]).

start_link() ->
    {ok, Conn} = erlmpd:connect(),
    {ok, spawn_link(?MODULE, wait_for_events, [Conn])}.

wait_for_events(Conn) ->
    Events = erlmpd:idle(Conn),
    gproc:send({p, l, erlmpc_subscriber}, {events, Events}),
    wait_for_events(Conn).
