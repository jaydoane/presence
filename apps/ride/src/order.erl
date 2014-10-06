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
    ?trace([Tid, Opts]),
    Rider = {rider, proplists:get_value(rider, Opts)},
    Address = proplists:get_value(address, Opts),
    gen_entity:subscribe(Rider, self()),
    {ok, processing, #order_data{tid=Tid, rider=Rider, address=Address}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_entity receives an event sent using
%% gen_entity:send_event/2, the instance of this function with the same
%% name as the current state name DataName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, Data) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData}
%% @end
%%--------------------------------------------------------------------
processing({completed, Hail}, #order_data{hail=Hail, old_hails=OldHails}=Data) ->
    ?trace([Hail]),
    {next_state, completed, Data#order_data{hail=undefined, old_hails=[Hail|OldHails]}};
processing({accepted, Hail}, #order_data{hail=Hail}=Data) ->
    ?trace([Hail]),
    {next_state, processing, Data};
processing({declined, Hail}, #order_data{hail=Hail, old_hails=OldHails}=Data) ->
    ?trace([Hail]),
    gen_entity:send_all_state_event(self(), {remove_sub, Hail}),
    {next_state, processing, Data#order_data{hail=undefined, old_hails=[Hail|OldHails]}};
processing({timed_out, Hail}, #order_data{hail=Hail, old_hails=OldHails}=Data) ->
    ?trace([Hail]),
    gen_entity:send_all_state_event(self(), {remove_sub, Hail}),
    {next_state, processing, Data#order_data{hail=undefined, old_hails=[Hail|OldHails]}}.

canceled(Event, Data) ->
    ?info("illegal state change ~p", [Event]),
    {next_state, canceled, Data}.

completed(Event, Data) ->
    ?info("illegal state change ~p", [Event]),
    {next_state, canceled, Data}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_entity receives an event sent using
%% gen_entity:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name DataName is called to
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
processing({cancel, _Reason}=Event, _From, #order_data{tid=Tid, rider=Rider}=Data) ->
    ?info("~p ~p", [Event, Tid]),
    gen_entity:send_all_state_event(Tid, {remove_sub, Rider}),
    {reply, {ok, canceled}, canceled, Data};
processing(Event, From, Data) ->
    ?info("illegal state change ~p ~p", [Event, From]),
    {next_state, processing, Data}.

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
handle_sync_event({set_hail, Hail}, _From, State, Data) ->
    {reply, ok, State, Data#order_data{hail=Hail}};
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
%% @spec handle_info(Info,State,Data)->
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
