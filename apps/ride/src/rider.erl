%%%-------------------------------------------------------------------
%%% @author Jay Doane <jay.s.doane@gmail.com>
%%% @copyright (C) 2014, Jay Doane
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by Jay Doane <jay.s.doane@gmail.com>
%%%-------------------------------------------------------------------
-module(rider).

-behaviour(gen_entity).

-include_lib("entity/include/log.hrl").

%% API
-export([create/1, start_link/2, order/2]).

%% gen_entity callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([observing/3, ordering/2]).

-define(SERVER, ?MODULE).

-record(rider_data, {tid, name, order, old_orders=[]}).

%%%===================================================================
%%% API
%%%===================================================================

create(Opts) ->
    Tid = counter:next_tid(?MODULE), % simulate persistent creation, client process blocks
    {ok, _Pid} = entity_sup:start_grandchild(Tid, Opts),
    {ok, Tid}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_entity process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Tid, Opts) ->
    ?info("~p ~p", [Tid, Opts]),
    gen_entity:start_link(Tid, Opts).

order(Tid, Opts) ->
    ?info("~p ~p", [Tid, Opts]),
    gen_entity:sync_send_event(Tid, {order, Opts}).

%%%===================================================================
%%% gen_entity callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_entity is started using gen_entity:start/[3,4] or
%% gen_entity:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, State, Data} |
%%                     {ok, State, Data, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Tid, Opts]) ->
    ?info("~p ~p", [Tid, Opts]),
    Name = proplists:get_value(name, Opts, "generic rider"),
    {ok, observing, #rider_data{tid=Tid, name=Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_entity receives an event sent using
%% gen_entity:send_event/2, the instance of this function with the same
%% name as the current state name State is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, Data) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData}
%% @end
%%--------------------------------------------------------------------
ordering({canceled, Order}, #rider_data{order=Order, old_orders=OldOrders}=Data) ->
    ?info("~p", [Order]),
    {next_state, observing, Data#rider_data{order=undefined, old_orders=[Order|OldOrders]}};
ordering({completed, Order}, #rider_data{order=Order, old_orders=OldOrders}=Data) ->
    ?info("~p", [Order]),
    {next_state, observing, Data#rider_data{order=undefined, old_orders=[Order|OldOrders]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_entity receives an event sent using
%% gen_entity:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name State is called to
%% handle the event.
%%
%% @spec state_name(Event, From, Data) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {reply, Reply, NextState, NextData} |
%%                   {reply, Reply, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData} |
%%                   {stop, Reason, Reply, NewData}
%% @end
%%--------------------------------------------------------------------
observing({order, Opts}, _From, #rider_data{tid=Tid}=Data) ->
    {ok, Order} = order:create(Tid, Opts),
    {reply, {ok, Order}, ordering, Data#rider_data{order=Order}};
observing(Event, _From, Data) ->
    ?info("illegal state change ~p", [Event]),
    {reply, {error, currently_observing}, observing, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_entity receives an event sent using
%% gen_entity:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, State, Data) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, State, Data) ->
    {next_state, State, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_entity receives an event sent using
%% gen_entity:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, State, Data) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {reply, Reply, NextState, NextData} |
%%                   {reply, Reply, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData} |
%%                   {stop, Reason, Reply, NewData}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, State, Data) ->
    Reply = ok,
    {reply, Reply, State, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_entity when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info, State, Data)->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State, Data) ->
    {next_state, State, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_entity when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_entity terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State, Data) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State, _Data) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Data, Extra) ->
%%                   {ok, StateName, NewData}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
