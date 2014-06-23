-module(session_server).

-behaviour(gen_server).

-include("session.hrl").

-export([start_link/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Tid) ->
    gen_server:start_link({global, Tid}, ?MODULE, [Tid], []).

stop(Tid) ->
    gen_server:call(gp:whereis(Tid), stop).


%% Behaviour functions

init([{Type,Id} = Tid]) ->
    ?info("~p", [Tid]),
    process_flag(trap_exit, true),
    {ok, #state{tid=Tid}}.


handle_call({add_listener, Tid}=_Msg, _From, #state{listeners=Listeners}=State) ->
    {reply, ok, State#state{listeners=[Tid|Listeners]}};

handle_call({remove_listener, Tid}=_Msg, _From, #state{listeners=Listeners}=State) ->
    {reply, ok, State#state{listeners=lists:delete(Tid, Listeners)}};

handle_call({add_transmitter, Tid}=_Msg, _From, #state{transmitters=Transmitters}=State) ->
    {reply, ok, State#state{transmitters=[Tid|Transmitters]}};

handle_call({remove_transmitter, Tid}=_Msg, _From, #state{transmitters=Transmitters}=State) ->
    {reply, ok, State#state{transmitters=lists:delete(Tid, Transmitters)}};
handle_call(stop, _From, State) ->
    {stop, normal, ok ,State};

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

handle_cast({notification, Notification}=_Msg, #state{notifications=Notifications}=State) ->
    %% info("handle_cast ~p", [_Msg]),
    {noreply, State#state{notifications=[Notification|Notifications]}};


handle_cast(_Msg, State) ->
    ?info("unhandled cast ~p", [_Msg]),
    {noreply, State}.


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
