-module(session_server).

-behaviour(gen_server).

-export([start_link/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tid, created=os:timestamp(), listeners=[], transmitters=[], notifications=[]}).

start_link(Tid) ->
    gen_server:start_link(?MODULE, [Tid], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).


%% Behaviour functions

init([Tid]) ->
    info("init ~p", [Tid]),
    process_flag(trap_exit, true),
    gp:register(Tid, self()),
    {ok, #state{tid=Tid}}.


handle_call({add_listener, Tid}=_Msg, _From, #state{listeners=Listeners}=State) ->
    {reply, ok, State#state{listeners=[Tid|Listeners]}};

handle_call({remove_listener, Tid}=_Msg, _From, #state{listeners=Listeners}=State) ->
    {reply, ok, State#state{listeners=lists:delete(Tid, Listeners)}};

handle_call({add_transmitter, Tid}=_Msg, _From, #state{transmitters=Transmitters}=State) ->
    {reply, ok, State#state{transmitters=[Tid|Transmitters]}};

handle_call({remove_transmitter, Tid}=_Msg, _From, #state{transmitters=Transmitters}=State) ->
    {reply, ok, State#state{transmitters=lists:delete(Tid, Transmitters)}};

handle_call(listeners, _From, #state{listeners=Listeners}=State) ->
    {reply, Listeners, State};

handle_call(transmitters, _From, #state{transmitters=Transmitters}=State) ->
    {reply, Transmitters, State};

handle_call(notifications, _From, #state{notifications=Notifications}=State) ->
    {reply, Notifications, State};

handle_call(_Msg, _From, State) ->
    info("unhandled call ~p", [_Msg]),
    {noreply, State}.


%% handle_cast({send, Msg}, #state{out=Out}=State) ->
%%     info("handle_cast ~p", [{send, Msg}]),
%%     {noreply, State#state{out=[Msg|Out]}};

%% handle_cast({add_listener, Tid}=_Msg, #state{listeners=Listeners}=State) ->
%%     {noreply, State#state{listeners=[Tid|Listeners]}};

%% handle_cast({remove_listener, Tid}=_Msg, #state{listeners=Listeners}=State) ->
%%     {noreply, State#state{listeners=lists:delete(Tid, Listeners)}};

%% handle_cast({add_transmitter, Tid}=_Msg, #state{transmitters=Transmitters}=State) ->
%%     {noreply, State#state{transmitters=[Tid|Transmitters]}};

%% handle_cast({remove_transmitter, Tid}=_Msg, #state{transmitters=Transmitters}=State) ->
%%     {noreply, State#state{transmitters=lists:delete(Tid, Transmitters)}};

handle_cast({notify_listeners, Notification}=_Msg, #state{listeners=Listeners}=State) ->
    %% info("handle_cast ~p", [_Msg]),
    [gp:cast(Tid, {notification, Notification}) || Tid <- Listeners],
    {noreply, State};

handle_cast({notification, Notification}=_Msg, #state{notifications=Notifications}=State) ->
    %% info("handle_cast ~p", [_Msg]),
    {noreply, State#state{notifications=[Notification|Notifications]}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    info("unhandled cast ~p", [_Msg]),
    {noreply, State}.


handle_info(_Msg, State) ->
    info("unhandled info ~p", [_Msg]),
    {noreply, State}.


terminate(_Reason, #state{tid=Tid, listeners=Listeners, transmitters=Transmitters}) ->
    info("terminate ~p  ~p", [Tid, _Reason]),
    %% [gp:cast(LTid, {remove_transmitter, Tid}) || LTid <- Listeners],
    %% [gp:cast(TTid, {remove_listener, Tid}) || TTid <- Transmitters],
    [ok = gp:call(LTid, {remove_transmitter, Tid}) || LTid <- Listeners],
    [ok = gp:call(TTid, {remove_listener, Tid}) || TTid <- Transmitters],
    gp:unregister(Tid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% -define(MILLION, 1000000).
%% sid() ->
%%     {Mega, Sec, Micro} = erlang:now(),
%%     Num = (Mega * ?MILLION * ?MILLION) + (Sec * ?MILLION) + Micro,
%%     Num.
    %% list_to_binary(integer_to_list(Num)).

info(Format, Data) ->
    lager:info("~p ~p ~s", [?MODULE, self(), io_lib:format(Format, Data)]).
    %%error_logger:info_msg("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
