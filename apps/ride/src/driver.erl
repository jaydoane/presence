%%%-------------------------------------------------------------------
%%% @author Jay Doane <jay.s.doane@gmail.com>
%%% @copyright (C) 2014, Jay Doane
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by Jay Doane <jay.s.doane@gmail.com>
%%%-------------------------------------------------------------------
-module(driver).

-behaviour(gen_entity).

-include_lib("util/include/log.hrl").

%% API
-export([create/1, start_link/2, hail/3, occupy/1, vacate/1]).

%% callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% states
-export([available/3, occupied/3, in_hail/2, in_hail/3]). % states

-define(SERVER, ?MODULE).

-record(driver_data, {tid, name, hail, old_hails=[]}).

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

hail(Tid, Order, Opts) ->
    gen_entity:sync_send_event(Tid, {hail, Order, Opts}).

occupy(Tid) ->
    gen_entity:sync_send_event(Tid, occupy).

vacate(Tid) ->
    gen_entity:sync_send_event(Tid, vacate).

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
    Name = proplists:get_value(name, Opts, "nameless driver"),
    {ok, available, #driver_data{tid=Tid, name=Name}}.

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
in_hail({completed,Hail}, #driver_data{hail=Hail, old_hails=OldHails}=Data) ->
    NextData = Data#driver_data{hail=undefined, old_hails=[Hail|OldHails]},
    {next_state, occupied, NextData};
in_hail({accepted,Hail}, #driver_data{hail=Hail}=Data) ->
    {next_state, in_hail, Data};
in_hail({declined,Hail}, #driver_data{hail=Hail, old_hails=OldHails}=Data) ->
    gen_entity:send_all_state_event(self(), {remove_sub, Hail}),
    NextData = Data#driver_data{hail=undefined, old_hails=[Hail|OldHails]},
    {next_state, available, NextData};
in_hail({timed_out,Hail}, #driver_data{hail=Hail, old_hails=OldHails}=Data) ->
    gen_entity:send_all_state_event(self(), {remove_sub, Hail}),
    NextData = Data#driver_data{hail=undefined, old_hails=[Hail|OldHails]},
    {next_state, available, NextData};
in_hail(Event, Data) ->
    ?trace([Event, Data]),
    {next_state, in_hail, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_entity receives an event sent using
%% gen_entity:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name State is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {reply, Reply, NextState, NextData} |
%%                   {reply, Reply, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData} |
%%                   {stop, Reason, Reply, NewData}
%% @end
%%--------------------------------------------------------------------
available(occupy, _From, Data) ->
    {reply, {ok, occupied}, occupied, Data};
available({hail, Order, Opts}, _From, #driver_data{tid=Tid}=Data) ->
    {ok, Hail} = hail:create(Order, Tid, Opts),
    {reply, {ok, Hail}, in_hail, Data#driver_data{hail=Hail}};
available(Event, _From, Data) ->
    ?info("illegal state change from available to ~p", [Event]),
    {reply, {error, currently_available}, available, Data}.

occupied(vacate, _From, Data) ->
    {reply, {ok, available}, available, Data};
occupied(Event, _From, Data) ->
    ?info("illegal state change from occupied to ~p", [Event]),
    {reply, {error, currently_occupied}, occupied, Data}.

in_hail({cancel,HailTid}, _From, #driver_data{hail=HailTid, old_hails=OldHails}=Data) ->
    NextData = Data#driver_data{hail=undefined, old_hails=[HailTid|OldHails]},
    {reply, {ok, available}, available, NextData};
in_hail(Event, _From, Data) ->
    ?info("illegal state change from in_hail to ~p", [Event]),
    {reply, {error, currently_in_hail}, in_hail, Data}.

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
%%                   {ok, State, NewData}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
