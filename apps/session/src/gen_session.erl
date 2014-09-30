-module(gen_session).

-behaviour(gen_server).

-include("session.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, stop/1]).

%% -export([add_listener/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,2},
     {handle_message,2}].

%% API

start_link(Tid, Opts) ->
    gen_server:start_link({global, Tid}, ?MODULE, [Tid, Opts], []).

stop(Tid) ->
    gen_server:cast(gp:whereis(Tid), stop).

%% Behaviour

init([{Type,_Id}=Tid, Opts]) ->
    ?info("~p ~p", [Tid, Opts]),
    process_flag(trap_exit, true),
    State = Type:init(Tid, Opts),
    %% {ok, State#state{tid=Tid}}.
    {ok, State#{tid => Tid, created => os:timestamp(), subs => []}}.


%% handle_call({add_listener, Tid}=_Msg, _From, State) ->
%%     {reply, ok, add_listener(Tid, State)};

%% handle_call({remove_listener, Tid}=_Msg, _From, State) ->
%%     {reply, ok, remove_listener(Tid, State)};

%% handle_call(listeners, _From, #state{listeners=Listeners}=State) ->
%%     {reply, Listeners, State};

handle_call(subs, _From, #{subs := Subs}=State) ->
    {reply, Subs, State};

%% handle_call(notifications, _From, #state{notifications=Notifications}=State) ->
%%     {reply, Notifications, State};

handle_call(_Msg, _From, State) ->
    ?info("unhandled call ~p", [_Msg]),
    {noreply, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
%% handle_cast({add_listener, Tid}=_Msg, State) ->
%%     {noreply, add_listener(Tid, State)};
handle_cast({add_sub, Tid}=_Msg, State) ->
    {noreply, add_sub(Tid, State)};

handle_cast({notify_subs, Message}, #{subs := Subs}=State) ->
    %% ?info("handle_cast ~p", [_Msg]),
    [gp:cast(Tid, {message, Message}) || {Tid,_Ref} <- Subs],
    {noreply, State};

handle_cast({message, Message}, #{tid := {Type,_Id}}=State) ->
    NewState = Type:handle_message(Message, State),
    {noreply, NewState};
    
%% handle_cast({notify_listeners, Notification}=_Msg, #state{listeners=Listeners}=State) ->
%%     %% ?info("handle_cast ~p", [_Msg]),
%%     [gp:cast(Tid, {notification, Notification}) || Tid <- Listeners],
%%     {noreply, State};

%% handle_cast({notify, Notification}=_Msg, #state{tid={Type,_Id}}=State) ->
%%     %% ?info("handle_cast ~p", [_Msg]),
%%     NewState = Type:handle_notification(Notification, State),
%%     {noreply, NewState};

handle_cast(_Msg, State) ->
    ?info("unhandled cast ~p", [_Msg]),
    {noreply, State}.


handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    ?info("Pid ~p 'DOWN' reason ~p", [Pid, Reason]),
    {noreply, remove_ref(Ref, State)};
    
handle_info(_Msg, State) ->
    ?info("unhandled info ~p", [_Msg]),
    {noreply, State}.


%% terminate(Reason, #state{tid=Tid}) ->
terminate(Reason, #{tid := Tid}) ->
    ?info("~p ~p", [Tid, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

%% @doc return updated state with {Ref, Tid} added to subs if not already present
add_sub(Tid, #{subs := Subs}=State) ->
    case proplists:get_value(Tid, Subs) of
        undefined ->
            case gp:whereis(Tid) of
                undefined ->
                    State;
                Pid ->
                    Ref = erlang:monitor(process, Pid),
                    State#{subs := [{Tid,Ref}|Subs]}
            end;
        _ ->
            State
    end.

%% %% @doc return updated state with {Tid, Ref} added to listeners if not already present
%% add_listener(Tid, #state{listeners=Listeners}=State) ->
%%     case proplists:get_value(Tid, Listeners) of
%%         undefined ->
%%             Pid = gp:whereis(Tid), % TODO: handle possible race if Tid process dies before here?
%%             Ref = erlang:monitor(process, Pid),
%%             State#state{listeners=[{Tid,Ref}|Listeners]};
%%         _ ->
%%             State
%%     end.

%% %% @doc return updated state with Tid removed from listeners if present
%% remove_listener(Tid, #state{listeners=Listeners}=State) ->
%%     Remove = [{T,R} || {T,R} <- Listeners, T =:= Tid],
%%     State#state{listeners=Listeners--Remove}.

%% %% @doc return updated state with Ref removed from listeners if present
%% remove_ref(Ref, #state{listeners=Listeners}=State) ->
%%     Remove = [{T,R} || {T,R} <- Listeners, R =:= Ref],
%%     ?info("removing ~p", [Remove]),
%%     State#state{listeners=Listeners--Remove}.

%% @doc return updated state with Ref removed from listeners if present
remove_ref(Ref, #{subs := Subs}=State) ->
    Remove = [{T,R} || {T,R} <- Subs, R =:= Ref],
    ?info("removing ~p", [Remove]),
    State#{subs := Subs--Remove}.
