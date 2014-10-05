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

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([observing/3, ordering/2]).

-define(SERVER, ?MODULE).

-record(rider_state, {tid, name, order, old_orders=[]}).

%%%===================================================================
%%% API
%%%===================================================================

create(Opts) ->
    Tid = counter:next_tid(?MODULE), % simulate persistent creation, client process blocks
    {ok, _Pid} = entity_sup:start_grandchild(Tid, Opts),
    {ok, Tid}.

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
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
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Tid, Opts]) ->
    ?info("~p ~p", [Tid, Opts]),
    Name = proplists:get_value(name, Opts, "generic rider"),
    {ok, observing, #rider_state{tid=Tid, name=Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
ordering({canceled, Order}, #rider_state{order=Order, old_orders=OldOrders}=State) ->
    ?info("~p", [Order]),
    {next_state, observing, State#rider_state{order=undefined, old_orders=[Order|OldOrders]}};
ordering({completed, Order}, #rider_state{order=Order, old_orders=OldOrders}=State) ->
    ?info("~p", [Order]),
    {next_state, observing, State#rider_state{order=undefined, old_orders=[Order|OldOrders]}}.

%% observing(order, #rider_state{tid=Tid}=State) ->
%%     OrderTid = order:create(Tid, []),
%%     {next_state, ordering, State#rider_state{order=OrderTid}};
%% observing(Event, State) ->
%%     ?info("illegal state change ~p", [Event]),
%%     {next_state, observing, State}.

%% ordering(observe, State) ->
%%     {next_state, observing, State};
%% ordering(Event, State) ->
%%     ?info("illegal state change ~p", [Event]),
%%     {next_state, ordering, State}.
%% state_name(_Event, State) ->
%%     {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
observing({order, Opts}, _From, #rider_state{tid=Tid}=State) ->
    {ok, OrderTid} = order:create(Tid, Opts),
    {reply, {ok, OrderTid}, ordering, State#rider_state{order=OrderTid}};
observing(Event, _From, State) ->
    ?info("illegal state change ~p", [Event]),
    {reply, {error, currently_observing}, observing, State}.

%% ordering({cancel, Order}, _From, #rider_state{order=Order, old_orders=OldOrders}=State) ->
%%     ?info("~p", [Order]),
%%     {reply, {ok, observing}, observing, State#rider_state{order=undefined, old_orders=[Order|OldOrders]};
%% ordering({complete, _OrderTid}, _From, State) ->
%%     %% FIXME
%%     {reply, {ok, observing}, observing, State};
%% ordering(Event, _From, State) ->
%%     ?info("illegal state change ~p", [Event]),
%%     {reply, {error, currently_ordering}, ordering, State}.

%% state_name(_Event, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
