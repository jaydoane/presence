%%%-------------------------------------------------------------------
%%% @author Jay Doane <jay.s.doane@gmail.com>
%%% @copyright (C) 2014, Jay Doane
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2014 by Jay Doane <jay.s.doane@gmail.com>
%%%-------------------------------------------------------------------
-module(hail).

-behaviour(gen_entity).

-include_lib("util/include/log.hrl").

%% API
-export([create/3, start_link/2, accept/1, decline/1, complete/1]).

%% callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% states
-export([hailing/2, hailing/3, accepted/3]).

-record(hail_data, {tid, order, driver, timer}).

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

accept(Hail) ->
    gen_entity:sync_send_event(Hail, accept).

decline(Hail) ->
    gen_entity:sync_send_event(Hail, decline).

complete(Hail) ->
    gen_entity:sync_send_event(Hail, complete).

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
    Timeout = proplists:get_value(timeout_ms, Opts, ?DEFAULT_HAIL_TIMEOUT_MS),
    Driver = {driver, proplists:get_value(driver, Opts)},
    gen_entity:subscribe(Driver, self()),
    Order = {order, proplists:get_value(order, Opts)},
    ok = order:set_hail(Order, Tid),
    gen_entity:subscribe(Order, self()),
    Timer = gen_entity:start_timer(Timeout, {server, Timeout}),
    {ok, hailing, #hail_data{tid=Tid, order=Order, driver=Driver, timer=Timer}}.

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
hailing({timeout, Ref, Msg}, Data) ->
    ?info("timeout ~p ~p", [Ref, Msg]),
    gen_entity:remove_subs(self()),
    {next_state, timed_out, Data};
hailing(Message, Data) ->
    ?info("illegal state transition request ~p", [Message]),
    {next_state, hailing, Data}.

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
hailing(accept, _From, Data) ->
    ?info("accept"),
    NewData = cancel_timer(Data),
    {reply, {ok, accepted}, accepted, NewData};
hailing(decline, _From, Data) ->
    ?info("decline"),
    NewData = cancel_timer(Data),
    gen_entity:remove_subs(self()),
    {reply, {ok, declined}, declined, NewData};
hailing(Message, _From, Data) ->
    ?info("illegal state transition request ~p", [Message]),
    {next_state, hailing, Data}.

accepted(complete, _From, Data) ->
    ?info("complete"),
    {reply, {ok, completed}, completed, Data};
accepted(cancel, _From, Data) ->
    ?info("cancel"),
    {reply, {ok, canceled}, canceled, Data}.

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

cancel_timer(#hail_data{tid=Tid, timer=Timer}=Data) ->
    Remaining = gen_entity:cancel_timer(Timer),
    ?info("~p with ~p remaining", [Tid, Remaining]),
    Data#hail_data{timer=undefined}.
