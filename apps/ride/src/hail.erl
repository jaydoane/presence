%%%-------------------------------------------------------------------
%%% @author Jay Doane <jay.s.doane@gmail.com>
%%% @copyright (C) 2014, Jay Doane
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2014 by Jay Doane <jay.s.doane@gmail.com>
%%%-------------------------------------------------------------------
-module(hail).

-behaviour(gen_fsm).

-include_lib("entity/include/log.hrl").

%% API
-export([create/3, start_link/2, accept/1, decline/1, complete/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([hailing/2, hailing/3, accepted/3]).

-record(state, {tid, order, driver, timer}).

-define(DEFAULT_HAIL_TIMEOUT_MS, 10000). % 10 sec.

%%%===================================================================
%%% API
%%%===================================================================

create(Order, Driver, Opts) ->
    Tid = counter:next_tid(?MODULE), % simulate persistent creation, client process blocks
    {ok, _Pid} = entity_sup:start_grandchild(Tid, [Driver|[Order|Opts]]),
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

accept(Hail) ->
    gen_entity:sync_send_event(Hail, accept).

decline(Hail) ->
    gen_entity:sync_send_event(Hail, decline).

complete(Hail) ->
    gen_entity:sync_send_event(Hail, complete).
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
    Timeout = proplists:get_value(timeout_ms, Opts, ?DEFAULT_HAIL_TIMEOUT_MS),
    Driver = {driver, proplists:get_value(driver, Opts)},
    gen_entity:send_all_state_event(self(), {subscribe, Driver}),
    Order = {order, proplists:get_value(order, Opts)},
    %% Order and Hail subscribe to each other
    gen_entity:send_all_state_event(self(), {subscribe, Order}),
    %% gen_entity:send_all_state_event(Order, {subscribe, Tid}),
    Timer = gen_entity:start_timer(Timeout, {server, Timeout}),
    {ok, hailing, #state{tid=Tid, order=Order, driver=Driver, timer=Timer}}.

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
%% hailing(accept, State) ->
%%     ?info("accept"),
%%     {next_state, accepted, State};
%% hailing(decline, State) ->
%%     ?info("decline"),
%%     {next_state, declined, State};
hailing({timeout, Ref, Msg}, State) ->
    ?info("timeout ~p ~p", [Ref, Msg]),
    gen_entity:remove_subs(self()),
    {next_state, timed_out, State};
hailing(Message, State) ->
    ?info("illegal state transition request ~p", [Message]),
    {next_state, hailing, State}.

%% accepted(complete, State) ->
%%     ?info("complete"),
%%     {next_state, completed, State};
%% accepted(cancel, State) ->
%%     ?info("cancel"),
%%     {next_state, canceled, State}.

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
hailing(accept, _From, State) ->
    ?info("accept"),
    NewState = cancel_timer(State),
    {reply, {ok, accepted}, accepted, NewState};
hailing(decline, _From, State) ->
    ?info("decline"),
    NewState = cancel_timer(State),
    gen_entity:remove_subs(self()),
    {reply, {ok, declined}, declined, NewState};
hailing(Message, _From, State) ->
    ?info("illegal state transition request ~p", [Message]),
    {next_state, hailing, State}.

accepted(complete, _From, State) ->
    ?info("complete"),
    {reply, {ok, completed}, completed, State};
accepted(cancel, _From, State) ->
    ?info("cancel"),
    {reply, {ok, canceled}, canceled, State}.


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

cancel_timer(#state{tid=Tid, timer=Timer}=State) ->
    Remaining = gen_entity:cancel_timer(Timer),
    ?info("~p with ~p remaining", [Tid, Remaining]),
    State#state{timer=undefined}.
