-module(session_server).

-behaviour(gen_server).

-include("session.hrl").

-export([start_link/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add_listener/2]).

%% API

start_link(Tid, Opts) ->
    gen_server:start_link({global, Tid}, ?MODULE, [Tid, Opts], []).

stop(Tid) ->
    gen_server:call(gp:whereis(Tid), stop).

%% Behaviour

init([{Type,_Id}=Tid, Opts]) ->
    ?info("~p ~p", [Tid, Opts]),
    process_flag(trap_exit, true),
    State = try
                erlang:apply(Type, init, [Tid, Opts])
            catch
                error:undef ->
                    #state{}
            end,
    {ok, State#state{tid=Tid}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok ,State};

handle_call({add_listener, Tid}=_Msg, _From, State) ->
    {reply, ok, add_listener(Tid, State)};

handle_call({remove_listener, Tid}=_Msg, _From, State) ->
    {reply, ok, remove_listener(Tid, State)};

handle_call(listeners, _From, #state{listeners=Listeners}=State) ->
    {reply, Listeners, State};

handle_call(notifications, _From, #state{notifications=Notifications}=State) ->
    {reply, Notifications, State};

handle_call(_Msg, _From, State) ->
    ?info("unhandled call ~p", [_Msg]),
    {noreply, State}.


handle_cast({add_listener, Tid}=_Msg, State) ->
    {noreply, add_listener(Tid, State)};

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


handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    ?info("Pid ~p 'DOWN' reason ~p", [Pid, Reason]),
    {noreply, remove_ref(Ref, State)};
    
handle_info(_Msg, State) ->
    ?info("unhandled info ~p", [_Msg]),
    {noreply, State}.


terminate(Reason, #state{tid=Tid}) ->
    ?info("~p ~p", [Tid, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

%% @doc return updated state with {Tid, Ref} added to listeners if not already present
add_listener(Tid, #state{listeners=Listeners}=State) ->
    case proplists:get_value(Tid, Listeners) of
        undefined ->
            Pid = gp:whereis(Tid), % TODO: handle possible race if Tid process dies before here?
            Ref = erlang:monitor(process, Pid),
            State#state{listeners=[{Tid,Ref}|Listeners]};
        _ ->
            State
    end.

%% @doc return updated state with Tid removed from listeners if present
remove_listener(Tid, #state{listeners=Listeners}=State) ->
    Remove = [{T,R} || {T,R} <- Listeners, T =:= Tid],
    State#state{listeners=Listeners--Remove}.

%% @doc return updated state with Ref removed from listeners if present
remove_ref(Ref, #state{listeners=Listeners}=State) ->
    Remove = [{T,R} || {T,R} <- Listeners, R =:= Ref],
    ?info("removing ~p", [Remove]),
    State#state{listeners=Listeners--Remove}.
