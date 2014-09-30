-module(driver_session).

-include_lib("session/include/session.hrl").

-compile(export_all).

start(Id) ->
    session:start({?MODULE, Id}).

init(_Tid, _Opts) ->
    #state{type_state=[{name, "Dale Driver"}]}.

notify(Notification, #state{tid={_,Id}, notifications=Notifications}=State) ->
    ?info("~p notified: ~p", [Id, Notification]),
    State#state{notifications=[Notification|Notifications]}.
