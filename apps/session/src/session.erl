-module(session).

-compile(export_all).

start_sessions(Count) ->
    [session_sup:start_child({session,N}) || N <- lists:seq(1,Count)].
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).


stop_sessions(Count) ->
    [session_sup:stop_child(gp:where({session,N})) || N <- lists:seq(1,Count)].

subscribe(TransmitterTid, ListenerTid) ->
    %% gp:cast(TransmitterTid, {add_listener, ListenerTid}),
    %% gp:cast(ListenerTid, {add_transmitter, TransmitterTid}).
    ok = gp:call(TransmitterTid, {add_listener, ListenerTid}),
    ok = gp:call(ListenerTid, {add_transmitter, TransmitterTid}).

unsubscribe(TransmitterTid, ListenerTid) ->
    ok = gp:call(TransmitterTid, {remove_listener, ListenerTid}),
    ok = gp:call(ListenerTid, {remove_transmitter, TransmitterTid}).

