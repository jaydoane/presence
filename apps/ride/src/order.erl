%%%-------------------------------------------------------------------
%%% @author Jay Doane <jay.s.doane@gmail.com>
%%% @copyright (C) 2014, Jay Doane
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2014 by Jay Doane <jay.s.doane@gmail.com>
%%%-------------------------------------------------------------------
-module(order).

-behaviour(gen_entity).

-include_lib("entity/include/log.hrl").

%% API
-export([create/2, start_link/2, cancel/2, set_hail/2]).

%% callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% states
-export([processing/2, processing/3, canceled/2, completed/2]).

-define(SERVER, ?MODULE).

-record(order_data, {tid, rider, address, hail, old_hails=[]}).

%%%===================================================================
%%% API
%%%===================================================================

create(Rider, Opts) ->
    Tid = counter:next_tid(?MODULE), % simulate persistent creation, client process blocks
    {ok, _Pid} = entity_sup:start_grandchild(Tid, [Rider|Opts]),
    {ok, Tid}.

cancel(Tid, Reason) ->
    ?trace([Tid, Reason]),
    gen_entity:sync_send_event(Tid, {cancel, Reason}).

set_hail(Tid, Hail) ->
    gen_entity:sync_send_all_state_event(Tid, {set_hail, Hail}).

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
    ?trace([Tid, Opts]),
    Rider = {rider, proplists:get_value(rider, Opts)},
    Address = proplists:get_value(address, Opts),
    gen_entity:send_all_state_event(self(), {subscribe, Rider}),
    {ok, processing, #order_data{tid=Tid, rider=Rider, address=Address}}.

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
processing({completed, Hail}, #order_data{hail=Hail, old_hails=OldHails}=State) ->
    ?trace([Hail]),
    {next_state, completed, State#order_data{hail=undefined, old_hails=[Hail|OldHails]}};
processing({accepted, Hail}, #order_data{hail=Hail}=State) ->
    ?trace([Hail]),
    {next_state, processing, State};
processing({declined, Hail}, #order_data{hail=Hail, old_hails=OldHails}=State) ->
    ?trace([Hail]),
    gen_entity:send_all_state_event(self(), {remove_sub, Hail}),
    {next_state, processing, State#order_data{hail=undefined, old_hails=[Hail|OldHails]}};
processing({timed_out, Hail}, #order_data{hail=Hail, old_hails=OldHails}=State) ->
    ?trace([Hail]),
    gen_entity:send_all_state_event(self(), {remove_sub, Hail}),
    {next_state, processing, State#order_data{hail=undefined, old_hails=[Hail|OldHails]}}.

canceled(Event, State) ->
    ?info("illegal state change ~p", [Event]),
    {next_state, canceled, State}.

completed(Event, State) ->
    ?info("illegal state change ~p", [Event]),
    {next_state, canceled, State}.

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
processing({cancel, _Reason}=Event, _From, #order_data{tid=Tid, rider=Rider}=State) ->
    ?info("~p ~p", [Event, Tid]),
    gen_entity:send_all_state_event(Tid, {remove_sub, Rider}),
    {reply, {ok, canceled}, canceled, State};
processing(Event, From, State) ->
    ?info("illegal state change ~p ~p", [Event, From]),
    {next_state, processing, State}.

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
handle_sync_event({set_hail, Hail}, _From, StateName, State) ->
    {reply, ok, StateName, State#order_data{hail=Hail}};
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
