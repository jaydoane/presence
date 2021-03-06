-module(session_sup).

-behaviour(supervisor).

-compile(export_all).

%% API
-export([start_link/0,
         start_link/1,
         start_domain_supervisor/1,
         start_child/2]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Type) ->
    supervisor:start_link({local, sup_of(Type)}, ?MODULE, [Type]).

%% start_child(Tid, Opts) ->
%%     supervisor:start_child(?MODULE, [Tid, Opts]).

start_child(Type, Opts) ->
    {ok, Pid} = ensure_sup_started(Type),
    supervisor:start_child(Pid, [Opts]).

init([]) -> % supervisor of domain entity supervisors
    RestartStrategy = {one_for_one, 1, 1}, % CHANGE FOR PRODUCTION
    {ok, {RestartStrategy, []}};

init([Type]) -> % supervisor of specific domain entity
    RestartStrategy = {simple_one_for_one, 1, 1}, % CHANGE FOR PRODUCTION
    ChildSpec = {Type, {Type, start_link, []}, transient, 5000, worker, [Type]},
    {ok, {RestartStrategy, [ChildSpec]}}.

%% init([]) ->
%%     RestartStrategy = {simple_one_for_one, 1, 1}, % CHANGE FOR PRODUCTION
%%     Children = [child(gen_session, worker)],
%%     {ok, {RestartStrategy, Children}}.

%% Internal

ensure_sup_started(Type) ->
    case whereis(sup_of(Type)) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            start_domain_supervisor(Type)
    end.

start_domain_supervisor(Type) ->
    Sup = sup_of(Type),
    ChildSpec = {Sup, {?MODULE, start_link, [Type]}, permanent, 5000, supervisor, [Sup]},
    supervisor:start_child(?MODULE, ChildSpec).

%% child(Module, ChildType) ->
%%     {Module, {Module, start_link, []}, transient, 100, ChildType, [Module]}.

sup_of(Type) ->
    list_to_atom(atom_to_list(Type) ++ "_sup").


