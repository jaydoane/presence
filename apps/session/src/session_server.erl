-module(session_server).

-behaviour(gen_server).

-include("session.hrl").

-export([start_link/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Tid, Opts) ->
    gen_server:start_link({global, Tid}, ?MODULE, [Tid, Opts], []).

stop(Tid) ->
    gen_server:call(gp:whereis(Tid), stop).


%% Behaviour functions

init([{Type,_Id}=Tid, Opts]) ->
    ?info("~p ~p", [Tid, Opts]),
    process_flag(trap_exit, true),
    TypeState = try
                    erlang:apply(Type, init, [Tid, Opts])
                catch
                    error:undef ->
                        undefined
                end,
    {ok, #state{tid=Tid, type_state=TypeState}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok ,State};

handle_call({add_listener, Tid}=_Msg, _From, #state{listeners=Listeners, refs=Refs}=State) ->
    case lists:member(Tid, Listeners) of
        false ->
            ListenerPid = gp:whereis(Tid),
            Ref = erlang:monitor(process, ListenerPid),
            {reply, ok, State#state{listeners=[Tid|Listeners], refs=[{Ref,Tid}|Refs]}};
        true ->
            {reply, ok, State}
    end;

handle_call({remove_listener, Tid}=_Msg, _From, #state{listeners=Listeners, refs=Refs}=State) ->
    ListenerRefs = [{R,T} || {R,T} <- Refs, T =:= Tid],
    {reply, ok, State#state{listeners=lists:delete(Tid, Listeners), refs=Refs--ListenerRefs}};

handle_call({add_transmitter, Tid}=_Msg, _From, #state{transmitters=Transmitters, refs=Refs}=State) ->
    case lists:member(Tid, Transmitters) of
        false ->
            TransmitterPid = gp:whereis(Tid),
            Ref = erlang:monitor(process, TransmitterPid),
            {reply, ok, State#state{transmitters=[Tid|Transmitters], refs=[{Ref,Tid}|Refs]}};
        true ->
            {reply, ok, State}
    end;

handle_call({remove_transmitter, Tid}=_Msg, _From,
            #state{transmitters=Transmitters, refs=Refs}=State) ->
    TransmitterRefs = [{R,T} || {R,T} <- Refs, T =:= Tid],
    {reply, ok, State#state{transmitters=lists:delete(Tid, Transmitters), refs=Refs--TransmitterRefs}};

handle_call(listeners, _From, #state{listeners=Listeners}=State) ->
    {reply, Listeners, State};

handle_call(transmitters, _From, #state{transmitters=Transmitters}=State) ->
    {reply, Transmitters, State};

handle_call(notifications, _From, #state{notifications=Notifications}=State) ->
    {reply, Notifications, State};

handle_call(_Msg, _From, State) ->
    ?info("unhandled call ~p", [_Msg]),
    {noreply, State}.


handle_cast({notify_listeners, Notification}=_Msg, #state{listeners=Listeners}=State) ->
    %% ?info("handle_cast ~p", [_Msg]),
    [gp:cast(Tid, {notification, Notification}) || Tid <- Listeners],
    {noreply, State};

handle_cast({notification, Notification}=_Msg,
            #state{tid={Type,_Id}, notifications=Notifications}=State) ->
    %% ?info("handle_cast ~p", [_Msg]),
    NewState = try
                   erlang:apply(Type, notify, [Notification, State])
               catch
                    error:undef ->
                       State#state{notifications=[Notification|Notifications]}
               end,
    {noreply, NewState};

handle_cast(_Msg, State) ->
    ?info("unhandled cast ~p", [_Msg]),
    {noreply, State}.


handle_info({'DOWN', Ref, process, Pid, Reason},
            #state{listeners=Listeners, transmitters=Transmitters, refs=Refs}=State) ->
    ?info("Pid ~p exited reason ~p", [Pid, Reason]),
    DownRefs = [{R,T} || {R,T} <- Refs, R =:= Ref],
    ?info("DownRefs ~p", [DownRefs]),
    DownTids = [T || {_R,T} <- DownRefs],
    ?info("DownTids ~p", [DownTids]),
    {noreply, State#state{listeners=Listeners--DownTids, transmitters=Transmitters--DownTids,
                          refs=Refs--DownRefs}};
    
handle_info(_Msg, State) ->
    ?info("unhandled info ~p", [_Msg]),
    {noreply, State}.


terminate(Reason, #state{tid=Tid, listeners=Listeners, transmitters=Transmitters}) ->
    ?info("~p ~p", [Tid, Reason]),
    [ok = gp:call(LTid, {remove_transmitter, Tid}) || LTid <- Listeners],
    [ok = gp:call(TTid, {remove_listener, Tid}) || TTid <- Transmitters],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
