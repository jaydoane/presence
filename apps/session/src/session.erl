-module(session).

-include("session.hrl").

-compile(export_all).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(Tid) ->
    start(Tid, []).

start(Tid, Opts) ->
    session_sup:start_child(Tid, Opts).

stop(Tid) ->
    session_server:stop(Tid).

start_count(Type, Count) ->
    [start({Type,N}) || N <- lists:seq(1,Count)].

stop_count(Type, Count) ->
    [stop(gp:where({Type,N})) || N <- lists:seq(1,Count)].

subscribe(TransmitterTid, ListenerTid) ->
    ok = gp:call(TransmitterTid, {add_listener, ListenerTid}).

unsubscribe(TransmitterTid, ListenerTid) ->
    ok = gp:call(TransmitterTid, {remove_listener, ListenerTid}).

