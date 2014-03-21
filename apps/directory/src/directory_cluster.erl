-module(directory_cluster).

-export([nodes/0,
         add/3,
         delete/2,
         create_type/1]).

nodes() ->
    [node() | erlang:nodes()].

add(Type, Id, Pid) ->
    rpc:multicall(?MODULE:nodes(), directory, add, [Type, Id, Pid]).

create_type(Type) when is_atom(Type) ->
    rpc:multicall(?MODULE:nodes(), directory, create_type, [Type]).

delete(Type, Id) ->
    rpc:multicall(?MODULE:nodes(), directory, delete, [Type, Id]).
