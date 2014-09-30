%%%-------------------------------------------------------------------
%%% @author Jay Doane <jay.s.doane@gmail.com>
%%% @copyright (C) 2014, Jay Doane
%%% @doc
%%% http://erlangcentral.org/wiki/index.php?title=Cascading_Behaviours
%%% @end
%%% Created : 22 Sep 2014 by Jay Doane <jay.s.doane@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_entity).

-behaviour(gen_fsm).

-include_lib("domain/include/log.hrl").
-include_lib("domain/include/entity.hrl").

%% API
-export([start_link/2, subs/1, state_name/1, send_subs_event/2]).

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

-record(data, {tid, created=os:timestamp(),
               module, entity_state_name, entity_state_data,
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

%% sync_state({_,_}=Tid, Event) ->
%%     sync_send_event(gp:whereis(Tid), Event).

subs({_,_}=Tid) ->
    sync_send_all_state_event(gp:whereis(Tid), subs).

state_name({_,_}=Tid) ->
    sync_send_all_state_event(gp:whereis(Tid), state_name).

send_subs_event({_,_}=Tid, Event) ->
    ?info("~p, ~p", [Tid, Event]),
    send_all_state_event(gp:whereis(Tid), {subs, Event}).

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
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([{Type,_Id}=Tid, Opts]) ->
    ?info("~p ~p", [Tid, Opts]),
    case Type:init([Tid, Opts]) of
        {ok, EntityStateName, EntityStateData} ->
            io:fwrite("    {ok, ~p, ~p}~n", [EntityStateName, EntityStateData]),
            StateData = #data{tid=Tid, module=Type, entity_state_name=EntityStateName,
                               entity_state_data=EntityStateData},
            {ok, state, StateData};
        {ok, EntityStateName, EntityStateData, Timeout} ->
            io:fwrite("    {ok, ~p, ~p, ~p}~n",
                      [EntityStateName, EntityStateData, Timeout]),
            StateData = #data{tid=Tid, module=Type, entity_state_name=EntityStateName,
                               entity_state_data=EntityStateData},
            {ok, state, StateData, Timeout};
        {stop, Reason} ->
            io:fwrite("    {stop, ~p}~n", [Reason]),
            {stop, Reason};
        Other ->
            io:fwrite("    ~p~n", [Other]),
            Other
    end.        

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
state(Event, #data{module=Module, entity_state_data=EntityStateData,
                    entity_state_name=EntityStateName}=StateData) ->
    io:fwrite("~p:~p(~p, ~p) ->~n",
        [Module, EntityStateName, Event, EntityStateData]),
    Result = Module:EntityStateName(Event, EntityStateData),
    handle_result(Result, state, StateData).

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
%% state_name(_Event, _From, State) ->
%%     Reply = ok,
%%     {reply, Reply, state_name, State}.
state(Event, From, #data{module=Module, entity_state_data=EntityStateData,
                          entity_state_name=EntityStateName}=StateData) ->
    io:fwrite("~p:~p(~p, ~p, ~p) ->~n",
        [Module, EntityStateName, Event, From, EntityStateData]),
    Result = Module:EntityStateName(Event, From, EntityStateData),
    handle_result(Result, state, StateData).

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
%% handle_event(_Event, StateName, State) ->
%%     {next_state, StateName, State}.

handle_event({subscribe, Tid}, StateName, State) ->
    {next_state, StateName, subscribe(Tid, State)};

handle_event({remove_sub, Sub}=Event, StateName, State) ->
    ?info("~p", [Event]),
    {next_state, StateName, remove_sub(Sub, State)};

handle_event({notify_subs, Notification}, StateName, #data{subs=Subs}=State) ->
    ?info("{notify_subs ~p}", [Notification]),
    [send_all_state_event(Pid, {notify, Notification}) || {_Tid,Pid,_Ref} <- Subs],
    {next_state, StateName, State};

handle_event(Event, StateName, #data{module=Module, entity_state_name=EntityStateName,
                                      entity_state_data=EntityStateData}=StateData) ->
    ?info("~p ~p", [Event, StateName]),
    io:fwrite("~p:handle_event(~p, ~p, ~p) ->~n",
              [Module, Event, EntityStateName, EntityStateData]),
    Result = Module:handle_event(Event, EntityStateName, EntityStateData),
    handle_result(Result, StateName, StateData).

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
%% handle_sync_event(_Event, _From, StateName, State) ->
%%     Reply = ok,
%%     {reply, Reply, StateName, State}.

handle_sync_event(subs, _From, StateName, #data{subs=Subs}=StateData) ->
    {reply, Subs, StateName, StateData};

handle_sync_event(state_name, _From, StateName,
                  #data{entity_state_name=EntityStateName}=StateData) ->
    {reply, EntityStateName, StateName, StateData};

handle_sync_event(Event, From, StateName,
                  #data{module=Module, entity_state_name=EntityStateName,
                         entity_state_data=EntityStateData}=StateData) ->
    io:fwrite("~p:handle_sync_event(~p, ~p, ~p, ~p) ->~n",
              [Module, Event, From, EntityStateName, EntityStateData]),
    Result = Module:handle_sync_event(Event, From, EntityStateName, EntityStateData),
    handle_result(Result, StateName, StateData).

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
%% handle_info(_Info, StateName, State) ->
%%     {next_state, StateName, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, StateName, State) ->
    ?info("Pid ~p 'DOWN' reason ~p", [Pid, Reason]),
    {next_state, StateName, remove_sub(Ref, State)};
    
handle_info(Info, StateName, #data{module=Module, entity_state_name=EntityStateName,
                                      entity_state_data=EntityStateData}=StateData) ->
    io:fwrite("~p:handle_info(~p, ~p, ~p) ->~n",
              [Module, Info, EntityStateName, EntityStateData]),
    Result = Module:handle_info(Info, EntityStateName, EntityStateData),
    handle_result(Result, StateName, StateData).

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
%% terminate(_Reason, _StateName, _State) ->
%%     ok.

terminate(Reason, _StateName, #data{module=Module, entity_state_name=EntityStateName,
                                    entity_state_data=EntityStateData}) ->
    io:fwrite("~p:terminate(~p, ~p, ~p) ->~n",
        [Module, Reason, EntityStateName, EntityStateData]),
    Module:terminate(Reason, EntityStateName, EntityStateData).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
%% code_change(_OldVsn, StateName, State, _Extra) ->
%%     {ok, StateName, State}.

code_change(OldVsn, StateName, #data{module=Module, entity_state_name=EntityStateName,
                                      entity_state_data=EntityStateData}=StateData, Extra) ->
    io:fwrite("~p:code_change(~p, ~p, ~p, ~p) ->~n",
        [Module, OldVsn, EntityStateName, EntityStateData, Extra]),
    case Module:code_change(OldVsn, EntityStateName, EntityStateData, Extra) of
        {ok, NewEntityStateName, NewEntityStateData} ->
            io:fwrite("    {ok, ~p, ~p}~n",
                NewEntityStateName, NewEntityStateData),
            NewStateData = StateData#data{entity_state_name = NewEntityStateName,
                                           entity_state_data = NewEntityStateData},
            {ok, StateName, NewStateData};
        Else ->
            Else
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_subs_if_entity_state_changed(_EntityStateName,
                                    #data{entity_state_name=_EntityStateName}) ->
    ok;
notify_subs_if_entity_state_changed(NewEntityStateName,
                                    #data{tid=Tid, entity_state_name=_EntityStateName,
                                           subs=Subs}) ->
    ?info("~p", [NewEntityStateName]),
    [send_event(P, {NewEntityStateName, Tid}) || {_T,P,_R} <- Subs],
    ok.

%% This function handles the common result set of callbacks.
 
handle_result({next_state, NewEntityStateName, NewEntityStateData}, StateName, StateData) ->
    io:fwrite("    {next_state, ~p, ~p}~n", [NewEntityStateName, NewEntityStateData]),
    NewStateData = StateData#data{entity_state_name =  NewEntityStateName,
                                   entity_state_data =NewEntityStateData},
    notify_subs_if_entity_state_changed(NewEntityStateName, StateData),
    {next_state, StateName, NewStateData};
handle_result({next_state, NewEntityStateName, NewEntityStateData, Timeout},
              StateName, StateData) ->
    io:fwrite("    {next_state, ~p, ~p, ~p}~n",
              [NewEntityStateName, NewEntityStateData, Timeout]),
    NewStateData = StateData#data{entity_state_name =  NewEntityStateName,
                                   entity_state_data =NewEntityStateData},
    notify_subs_if_entity_state_changed(NewEntityStateName, StateData),
    {next_state, StateName, NewStateData, Timeout};
handle_result({reply, Reply, NewEntityStateName, NewEntityStateData},
              StateName, StateData) ->
    io:fwrite("    {reply, ~p, ~p, ~p}~n",
              [Reply, NewEntityStateName, NewEntityStateData]),
    NewStateData = StateData#data{entity_state_name =  NewEntityStateName,
                                   entity_state_data =NewEntityStateData},
    notify_subs_if_entity_state_changed(NewEntityStateName, StateData),
    {reply, Reply, StateName, NewStateData};
handle_result({reply, Reply, NewEntityStateName, NewEntityStateData, Timeout},
              StateName, StateData) ->
    io:fwrite("    {reply, ~p, ~p, ~p, ~p}~n",
              [Reply, NewEntityStateName, NewEntityStateData, Timeout]),
    NewStateData = StateData#data{entity_state_name =  NewEntityStateName,
                                   entity_state_data =NewEntityStateData},
    notify_subs_if_entity_state_changed(NewEntityStateName, StateData),
    {reply, Reply, StateName, NewStateData, Timeout};
handle_result({stop, Reason, NewEntityStateData}, _StateName, StateData) ->
    io:fwrite("    {stop, ~p, ~p}~n", [Reason, NewEntityStateData]),
    NewStateData = StateData#data{entity_state_data =NewEntityStateData},
    {stop, Reason, NewStateData};
handle_result({stop, Reason, Reply, NewEntityStateData},
              _StateName, StateData) ->
    io:fwrite("    {stop, ~p, ~p, ~p}~n", [Reason, Reply, NewEntityStateData]),
    NewStateData = StateData#data{entity_state_data =NewEntityStateData},
    {stop, Reason, Reply, NewStateData};
handle_result(Other, _StateName, _StateData) ->
    io:fwrite("    ~p~n", [Other]),
    Other.

%% subscriptions and notifications

subscribe(Tid, #data{subs=Subs}=State) ->
    ?info("~p", [Tid]),
    case proplists:get_value(Tid, Subs) of
        undefined ->
            case gp:whereis(Tid) of
                undefined ->
                    State;
                Pid ->
                    Ref = erlang:monitor(process, Pid),
                    State#data{subs = [{Tid,Pid,Ref}|Subs]}
            end;
        _ ->
            State
    end.

remove_sub({_Type,_Id}=Tid, #data{subs=Subs}=State) ->
    Remove = [{T,P,R} || {T,P,R} <- Subs, T =:= Tid],
    remove(Remove, Subs, State);
remove_sub(Ref, #data{subs=Subs}=State) when is_reference(Ref) ->
    Remove = [{T,P,R} || {T,P,R} <- Subs, R =:= Ref],
    remove(Remove, Subs, State).
    
remove(Remove, Subs, State) ->
    ?info("removing ~p", [Remove]),
    State#data{subs = Subs--Remove}.
    
