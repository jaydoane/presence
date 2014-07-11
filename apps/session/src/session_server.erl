-module(session_server).

-behaviour(gen_server).

-include("session.hrl").

-export([start_link/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add_listener/2]).

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

handle_call({add_listener, Tid}=_Msg, _From, State) ->
    {reply, ok, add_listener(Tid, State)};

handle_call({remove_listener, Tid}=_Msg, _From, #state{listeners=Listeners, ref_tids=RefTids}=State) ->
    ListenerRefTids = [{R,T} || {R,T} <- RefTids, T =:= Tid],
    {reply, ok, State#state{listeners=Listeners--Tid, ref_tids=RefTids--ListenerRefTids}};

handle_call(listeners, _From, #state{listeners=Listeners}=State) ->
    {reply, Listeners, State};

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
            #state{listeners=Listeners, ref_tids=RefTids}=State) ->
    ?info("Pid ~p exited reason ~p", [Pid, Reason]),
    DownRefTids = [{R,T} || {R,T} <- RefTids, R =:= Ref],
    ?info("DownRefTids ~p", [DownRefTids]),
    DownTids = [T || {_R,T} <- DownRefTids],
    ?info("DownTids ~p", [DownTids]),
    {noreply, State#state{listeners=Listeners--DownTids, ref_tids=RefTids--DownRefTids}};
    
handle_info(_Msg, State) ->
    ?info("unhandled info ~p", [_Msg]),
    {noreply, State}.


terminate(Reason, #state{tid=Tid}) ->
    ?info("~p ~p", [Tid, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

%% @doc return updated State with listener and monitor ref added if not already present
add_listener(Tid, #state{listeners=Listeners, ref_tids=RefTids}=State) ->
    case lists:member(Tid, Listeners) of
        false ->
            ListenerPid = gp:whereis(Tid),
            Ref = erlang:monitor(process, ListenerPid),
            State#state{listeners=[Tid|Listeners], ref_tids=[{Ref,Tid}|RefTids]};
        true ->
            State
    end.
