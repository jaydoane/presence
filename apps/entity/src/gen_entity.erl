%%%-------------------------------------------------------------------
%%% @author Jay Doane <jay.s.doane@gmail.com>
%%% @copyright (C) 2014, Jay Doane
%%% @doc
%%%
%%% Because the term `state' generally refers to the current state _name_ that a FSM is in
%%% rather than the state data, we use the variable State to refer to the state name and
%%% Data to refer to state data. This is unlike typical non-FSM based servers, where
%%% State refers to state data, and the state name is not explicit.
%%%
%%% http://erlangcentral.org/wiki/index.php?title=Cascading_Behaviours
%%% @end
%%% Created : 22 Sep 2014 by Jay Doane <jay.s.doane@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_entity).

-behaviour(gen_fsm).

-include_lib("util/include/log.hrl").
-include_lib("entity/include/entity.hrl").

-define(DEFAULT_IDLE_TIMEOUT, 15*60*1000).

%% API
-export([start_link/2, subscribe/2, subs/1, state/1, data/1, send_subs_event/2, remove_subs/1]).

%% gen_fsm callbacks
-export([init/1, state/2, state/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% gen_fsm passthroughs
-export([send_event/2, sync_send_event/2, sync_send_event/3, send_all_state_event/2,
         sync_send_all_state_event/2, sync_send_all_state_event/3, start_timer/2,
         send_event_after/2, cancel_timer/1, reply/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},{handle_event,3},{handle_sync_event,4},
     {handle_info,3}, {terminate,3},{code_change,4}].

-record(gen_data, {tid, created=os:timestamp(),
                   module, entity_state, entity_data,
                   idle_timer, idle_timeout,
                   subs=[] :: [sub()]}).

%%%===================================================================
%%% API
%%%===================================================================

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
    gen_fsm:start_link({global, Tid}, ?MODULE, [Tid, Opts], []).

subscribe(Subscriber, Publisher) ->
    gen_entity:send_all_state_event(Publisher, {subscribe, Subscriber}).

subs({_,_}=Tid) ->
    sync_send_all_state_event(Tid, subs).

state({_,_}=Tid) ->
    sync_send_all_state_event(Tid, state).

data({_,_}=Tid) ->
    sync_send_all_state_event(Tid, data).

send_subs_event({_,_}=Tid, Event) ->
    ?info("~p, ~p", [Tid, Event]),
    send_all_state_event(Tid, {subs, Event}).

remove_subs(Tid) ->
    send_all_state_event(Tid, remove_subs).

%%%===================================================================
%%% gen_fsm passthroughs with name lookup conversions for Tids
%%%===================================================================

send_event({_,_}=Tid, Event) ->
    send_event(gp:whereis(Tid), Event);
send_event(Name, Event) ->
    gen_fsm:send_event(Name, Event).

sync_send_event({_,_}=Tid, Event) ->
    sync_send_event(gp:whereis(Tid), Event);
sync_send_event(Name, Event) ->
    gen_fsm:sync_send_event(Name, Event).

sync_send_event({_,_}=Tid, Event, Timeout) ->
    sync_send_event(gp:whereis(Tid), Event, Timeout);
sync_send_event(Name, Event, Timeout) ->
    gen_fsm:sync_send_event(Name, Event, Timeout).

send_all_state_event({_,_}=Tid, Event) ->
    send_all_state_event(gp:whereis(Tid), Event);
send_all_state_event(Name, Event) ->
    gen_fsm:send_all_state_event(Name, Event).

sync_send_all_state_event({_,_}=Tid, Event) ->
    sync_send_all_state_event(gp:whereis(Tid), Event);
sync_send_all_state_event(Name, Event) ->
    gen_fsm:sync_send_all_state_event(Name, Event).

sync_send_all_state_event({_,_}=Tid, Event, Timeout) ->
    sync_send_all_state_event(gp:whereis(Tid), Event, Timeout);
sync_send_all_state_event(Name, Event, Timeout) ->
    gen_fsm:sync_send_all_state_event(Name, Event, Timeout).

start_timer(Time, Msg) ->
    gen_fsm:start_timer(Time, Msg).

send_event_after(Time, Event) ->
    gen_fsm:send_event_after(Time, Event).

cancel_timer(Ref) ->
    gen_fsm:cancel_timer(Ref).

reply(Caller, Reply) ->
    gen_fsm:reply(Caller, Reply).

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
%% @spec init(Args) -> {ok, State, Data} |
%%                     {ok, State, Data, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([{Type,_Id}=Tid, Opts]) ->
    ?info("~p ~p", [Tid, Opts]),
    IdleTimout = proplists:get_value(idle_timeout, Opts, ?DEFAULT_IDLE_TIMEOUT),
    case Type:init([Tid, Opts]) of
        {ok, EntityState, EntityData} ->
            ?debug("    {ok, ~p, ~p}~n", [EntityState, EntityData]),
            Data = #gen_data{tid=Tid, module=Type, entity_state=EntityState,
                             entity_data=EntityData, idle_timeout=IdleTimout},
            {ok, state, reset_idle_timer(Data)};
        {ok, EntityState, EntityData, Timeout} ->
            ?debug("    {ok, ~p, ~p, ~p}~n", [EntityState, EntityData, Timeout]),
            Data = #gen_data{tid=Tid, module=Type, entity_state=EntityState,
                             entity_data=EntityData, idle_timeout=IdleTimout},
            {ok, state, reset_idle_timer(Data), Timeout};
        {stop, Reason} ->
            ?debug("    {stop, ~p}~n", [Reason]),
            {stop, Reason};
        Other ->
            ?debug("    ~p~n", [Other]),
            Other
    end.        

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name State is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData}
%% @end
%%--------------------------------------------------------------------
state({timeout, Ref, idle}, Data) ->
    ?trace([timeout, idle, Ref]),
    {stop, normal, Data};
state(Event, #gen_data{module=Module, entity_data=EntityData,
                       entity_state=EntityState}=Data) ->
    ?debug("~p:~p(~p, ~p) ->~n", [Module, EntityState, Event, EntityData]),
    Result = Module:EntityState(Event, EntityData),
    handle_result(Result, state, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
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
%% state_name(_Event, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, state_name, State}.
state(Event, From, #gen_data{module=Module, entity_data=EntityData,
                             entity_state=EntityState}=Data) ->
    ?debug("~p:~p(~p, ~p, ~p) ->~n", [Module, EntityState, Event, From, EntityData]),
    Result = Module:EntityState(Event, From, EntityData),
    handle_result(Result, state, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, State, Data) ->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData}
%% @end
%%--------------------------------------------------------------------
%% handle_event(_Event, State, Data) ->
%%     {next_state, State, Data}.

handle_event({subscribe, Name}, State, Data) ->
    {next_state, State, do_subscribe(Name, Data)};

handle_event(remove_subs, State, Data) ->
    {next_state, State, Data#gen_data{subs=[]}};

handle_event({remove_sub, Sub}=Event, State, #gen_data{tid=Tid}=Data) ->
    ?trace([Tid, Event]),
    {next_state, State, remove_sub(Sub, Data)};

handle_event({notify_subs, Notification}, State, #gen_data{subs=Subs}=Data) ->
    ?info("{notify_subs ~p}", [Notification]),
    [send_all_state_event(Pid, {notify, Notification}) || {_Tid,Pid,_Ref} <- Subs],
    {next_state, State, Data};

handle_event(Event, State, #gen_data{module=Module, entity_state=EntityState,
                                     entity_data=EntityData}=Data) ->
    ?info("~p ~p", [Event, State]),
    ?debug("~p:handle_event(~p, ~p, ~p) ->~n", [Module, Event, EntityState, EntityData]),
    Result = Module:handle_event(Event, EntityState, EntityData),
    handle_result(Result, State, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
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
%% handle_sync_event(_Event, _From, State, Data) ->
%%     Reply = ok,
%%     {reply, Reply, State, Data}.

handle_sync_event(subs, _From, State, #gen_data{subs=Subs}=Data) ->
    {reply, Subs, State, Data};

handle_sync_event(state, _From, State, #gen_data{entity_state=EntityState}=Data) ->
    {reply, EntityState, State, Data};

handle_sync_event(data, _From, State, #gen_data{entity_data=EntityData}=Data) ->
    {reply, EntityData, State, Data};

handle_sync_event(Event, From, State, #gen_data{module=Module, entity_state=EntityState,
                                                entity_data=EntityData}=Data) ->
    ?debug("~p:handle_sync_event(~p, ~p, ~p, ~p) ->~n",
              [Module, Event, From, EntityState, EntityData]),
    Result = Module:handle_sync_event(Event, From, EntityState, EntityData),
    handle_result(Result, State, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,State,Data)->
%%                   {next_state, NextState, NextData} |
%%                   {next_state, NextState, NextData, Timeout} |
%%                   {stop, Reason, NewData}
%% @end
%%--------------------------------------------------------------------
%% handle_info(_Info, State, Data) ->
%%     {next_state, State, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State, Data) ->
    ?info("Pid ~p 'DOWN' reason ~p", [Pid, Reason]),
    {next_state, State, remove_sub(Ref, Data)};

handle_info(Info, State, #gen_data{module=Module, entity_state=EntityState,
                                   entity_data=EntityData}=Data) ->
    ?debug("~p:handle_info(~p, ~p, ~p) ->~n",
              [Module, Info, EntityState, EntityData]),
    Result = Module:handle_info(Info, EntityState, EntityData),
    handle_result(Result, State, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State, Data) -> void()
%% @end
%%--------------------------------------------------------------------
%% terminate(_Reason, _State, _Data) ->
%%     ok.

terminate(Reason, _State, #gen_data{module=Module, entity_state=EntityState,
                                    entity_data=EntityData}) ->
    ?debug("~p:terminate(~p, ~p, ~p) ->~n",
              [Module, Reason, EntityState, EntityData]),
    Module:terminate(Reason, EntityState, EntityData).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Data, Extra) ->
%%                   {ok, State, NewData}
%% @end
%%--------------------------------------------------------------------
%% code_change(_OldVsn, State, Data, _Extra) ->
%%     {ok, State, Data}.

code_change(OldVsn, State, #gen_data{module=Module, entity_state=EntityState,
                                     entity_data=EntityData}=Data, Extra) ->
    ?debug("~p:code_change(~p, ~p, ~p, ~p) ->~n",
              [Module, OldVsn, EntityState, EntityData, Extra]),
    case Module:code_change(OldVsn, EntityState, EntityData, Extra) of
        {ok, NewEntityState, NewEntityData} ->
            ?debug("    {ok, ~p, ~p}~n", [NewEntityState, NewEntityData]),
            NewData = Data#gen_data{entity_state = NewEntityState,
                                    entity_data = NewEntityData},
            {ok, State, NewData};
        Else ->
            Else
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

notify_subs_if_entity_state_changed_test() ->
    {ok, 0} = notify_subs_if_entity_state_changed(state, #gen_data{entity_state=state}),
    Tid = {foo,1},
    {ok, 1} = notify_subs_if_entity_state_changed(
                state, #gen_data{tid=Tid, entity_state=different_state,
                                 subs=[{Tid,self(),make_ref()}]}),
    ok.
-endif.


notify_subs_if_entity_state_changed(_EntityState, #gen_data{entity_state=_EntityState}) ->
    {ok, 0};
notify_subs_if_entity_state_changed(NewEntityState, #gen_data{tid=Tid, entity_state=_EntityState,
                                                              subs=Subs}) ->
    ?info("~p", [NewEntityState]),
    Count = length([send_event(P, {NewEntityState, Tid}) || {_T,P,_R} <- Subs]),
    {ok, Count}.

reset_idle_timer_if_state_changed(EntityState, #gen_data{entity_state=EntityState}=Data) ->
    Data;
reset_idle_timer_if_state_changed(_NewEntityState, #gen_data{entity_state=_EntityState}=Data) ->
    reset_idle_timer(Data).

reset_idle_timer(#gen_data{idle_timer=undefined, idle_timeout=IdleTimeout}=Data) ->
    NewTimer = gen_entity:start_timer(IdleTimeout, idle),
    Data#gen_data{idle_timer=NewTimer, idle_timeout=IdleTimeout};
reset_idle_timer(#gen_data{idle_timer=Timer, idle_timeout=IdleTimeout}=Data) ->
    _Remaining = cancel_timer(Timer),
    NewTimer = gen_entity:start_timer(IdleTimeout, idle),
    Data#gen_data{idle_timer=NewTimer, idle_timeout=IdleTimeout}.

%% This function handles the common result set of callbacks.

handle_result({next_state, NewEntityState, NewEntityData}, State, Data) ->
    ?debug("    {next_state, ~p, ~p}~n", [NewEntityState, NewEntityData]),
    NewData = Data#gen_data{entity_state = NewEntityState, entity_data = NewEntityData},
    notify_subs_if_entity_state_changed(NewEntityState, Data),
    {next_state, State, reset_idle_timer_if_state_changed(NewEntityState, NewData)};
handle_result({next_state, NewEntityState, NewEntityData, Timeout}, State, Data) ->
    ?debug("    {next_state, ~p, ~p, ~p}~n", [NewEntityState, NewEntityData, Timeout]),
    NewData = Data#gen_data{entity_state = NewEntityState, entity_data = NewEntityData},
    notify_subs_if_entity_state_changed(NewEntityState, Data),
    {next_state, State, reset_idle_timer_if_state_changed(NewEntityState, NewData), Timeout};
handle_result({reply, Reply, NewEntityState, NewEntityData}, State, Data) ->
    ?debug("    {reply, ~p, ~p, ~p}~n", [Reply, NewEntityState, NewEntityData]),
    NewData = Data#gen_data{entity_state = NewEntityState, entity_data = NewEntityData},
    notify_subs_if_entity_state_changed(NewEntityState, Data),
    {reply, Reply, State, reset_idle_timer_if_state_changed(NewEntityState, NewData)};
handle_result({reply, Reply, NewEntityState, NewEntityData, Timeout}, State, Data) ->
    ?debug("    {reply, ~p, ~p, ~p, ~p}~n", [Reply, NewEntityState, NewEntityData, Timeout]),
    NewData = Data#gen_data{entity_state = NewEntityState, entity_data = NewEntityData},
    notify_subs_if_entity_state_changed(NewEntityState, Data),
    {reply, Reply, State, reset_idle_timer_if_state_changed(NewEntityState, NewData), Timeout};
handle_result({stop, Reason, NewEntityData}, _State, Data) ->
    ?debug("    {stop, ~p, ~p}~n", [Reason, NewEntityData]),
    NewData = Data#gen_data{entity_data = NewEntityData},
    {stop, Reason, NewData};
handle_result({stop, Reason, Reply, NewEntityData}, _State, Data) ->
    ?debug("    {stop, ~p, ~p, ~p}~n", [Reason, Reply, NewEntityData]),
    NewData = Data#gen_data{entity_data = NewEntityData},
    {stop, Reason, Reply, NewData};
handle_result(Other, _State, _Data) ->
    ?debug("    ~p~n", [Other]),
    Other.

%% subscriptions and notifications

do_subscribe(GName, #gen_data{tid=Tid, subs=Subs}=State) ->
    ?info("~p subscribe ~p", [Tid, GName]),
    case proplists:get_value(Tid, Subs) of
        undefined ->
            case gp:whereis(GName) of
                undefined ->
                    State;
                Pid ->
                    Ref = erlang:monitor(process, Pid),
                    State#gen_data{subs = [{GName,Pid,Ref}|Subs]}
            end;
        _ ->
            State
    end.

remove_sub({_Type,_Id}=Tid, #gen_data{subs=Subs}=State) ->
    Remove = [{T,P,R} || {T,P,R} <- Subs, T =:= Tid],
    remove(Remove, Subs, State);
remove_sub(Ref, #gen_data{subs=Subs}=State) when is_reference(Ref) ->
    Remove = [{T,P,R} || {T,P,R} <- Subs, R =:= Ref],
    remove(Remove, Subs, State).

remove(Remove, Subs, State) ->
    ?info("removing ~p", [Remove]),
    State#gen_data{subs = Subs--Remove}.

