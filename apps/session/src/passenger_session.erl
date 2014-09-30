-module(passenger_session).

-behaviour(gen_session).

-include_lib("session/include/session.hrl").

-compile(export_all).

start(Id) ->
    session:start({?MODULE, Id}).

init(_Tid, _Opts) ->
    %% #state{type_state=#{name => "Pam Passenger"}}.
    #{type_state => #{name => "Pam Passenger"}}.

handle_message(Message, #{tid := {_,Id}}=State) ->
    ?info("~p messaged ~p", [Id, Message]),
    State.
