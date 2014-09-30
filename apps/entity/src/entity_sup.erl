-module(entity_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, start_grandchild/2]).

%% Callbacks
-export([init/1]).

%% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Type) ->
    supervisor:start_link({local, sup_of(Type)}, ?MODULE, [Type]).

start_grandchild({Type,_}=Tid, Opts) ->
    {ok, Pid} = ensure_sup_started(Type),
    supervisor:start_child(Pid, [Tid, Opts]).

%% Callbacks

init([]) -> % umbrella supervisor of entity supervisors
    RestartStrategy = {one_for_one, 1, 1}, % CHANGE FOR PRODUCTION
    {ok, {RestartStrategy, []}};

init([Type]) -> % supervisor of specific entity type
    RestartStrategy = {simple_one_for_one, 1, 1}, % CHANGE FOR PRODUCTION
    ChildSpec = {Type, {Type, start_link, []}, transient, 5000, worker, [Type]},
    {ok, {RestartStrategy, [ChildSpec]}}.

%% Internal

ensure_sup_started(Type) ->
    case whereis(sup_of(Type)) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            start_entity_supervisor(Type)
    end.

start_entity_supervisor(Type) ->
    Sup = sup_of(Type),
    ChildSpec = {Sup, {?MODULE, start_link, [Type]}, permanent, 5000, supervisor, [Sup]},
    supervisor:start_child(?MODULE, ChildSpec).

sup_of(Type) ->
    list_to_atom(atom_to_list(Type) ++ "_sup").
