-module(hail_session).

-include_lib("session/include/session.hrl").

-compile(export_all).

start(Id, PassengerId, DriverId) ->
    session:start({?MODULE, Id}, [{passenger,PassengerId},{driver,DriverId}]).

init(Tid, [{passenger,PassengerId},{driver,DriverId}]) ->
    session:subscribe({passenger,PassengerId}, {driver,DriverId}),
    session:subscribe({driver,DriverId}, {passenger,PassengerId}),

    session:subscribe_async({passenger,PassengerId}, Tid),
    session:subscribe_async({driver,DriverId}, Tid),

    %% session:subscribe(Tid, {passenger,PassengerId}),
    %% session:subscribe(Tid, {driver,DriverId}),
    State1 = session_server:add_listener({passenger,PassengerId}, #state{}),
    State2 = session_server:add_listener({driver,DriverId}, State1),

    State2#state{type_state=[{passenger,PassengerId},{driver,DriverId}]}.

notify(Notification, #state{tid={_,Id}, notifications=Notifications}=State) ->
    ?info("~p notified: ~p", [Id, Notification]),
    State#state{notifications=[Notification|Notifications]}.

test() ->
    {ok, _DriverPid} = driver:start(1),
    {ok, _PassengerPid} = passenger:start(2),
    {ok, _HailPid} = hail:start(3,2,1),
    ok.
