-module(order_session).

-behaviour(gen_session).

-include_lib("session/include/session.hrl").

-compile(export_all).

start(Id, ParentTid) ->
    session:start({?MODULE, Id}, [{parent,ParentTid}]).

init(Tid, Opts) ->
    ParentTid = proplists:get_value(parent, Opts),
    gp:cast(Tid, {add_sub, ParentTid}),
    gp:cast(ParentTid, {add_sub, Tid}),
    #{type_state => #{pickup_address => "San Francisco, CA",
                      parent => ParentTid,
                      state => processing}}.

handle_message(cancel=Message, #{tid := {_,Id}, type_state := TypeState}=State) ->
    ?info("~p messaged ~p", [Id, Message]),
    State#{type_state := TypeState#{state := canceled}};
    
handle_message(Message, #{tid := {_,Id}}=State) ->
    ?info("~p messaged ~p", [Id, Message]),
    State.
