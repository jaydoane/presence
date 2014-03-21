-module(directory).

-export([create_type/1,
         get_types/0,
         add/2,
         add/3,
         get/2,
         delete/2,
         get_by_node/2,
         copy_from_node/1,
         remove_node/1]).

-record(dir, {id,
              node,
              pid,
              ts=os:timestamp()}).

-define(TABLE_PREFIX, "_dir_type_").

create_type(Type) when is_atom(Type) ->
    mnesia:create_table(
      type_to_table(Type), 
      [{index, [node]},
       {record_name, dir},
       {attributes, record_info(fields, dir)}]).

get_types() ->
    [table_to_type(T) || 
        T <- mnesia:system_info(tables), string:str(atom_to_list(T), ?TABLE_PREFIX) =:= 1].

add(Type, #dir{}=Dir) ->
    {atomic, ok} = 
        mnesia:transaction(fun() -> mnesia:write(type_to_table(Type), Dir, write) end),
    ok;
add(Type, Id) ->
    add(Type, Id, self()). %% horrible hack

add(Type, Id, Pid) ->
    {atomic, ok} = 
        mnesia:transaction(
          fun() -> mnesia:write(
                     type_to_table(Type), #dir{id=Id, node=node(Pid), pid=Pid}, write) end),
    ok.

get(Type, Id) ->
    case mnesia:transaction(
           fun() -> mnesia:read(type_to_table(Type), Id) end) of
        {atomic, [#dir{id=Id, node=Node, pid=Pid, ts=TS}]} ->
            {ok, Pid, Node, TS};
        {atomic, []} ->
            {not_found, {key, Id}};
        {aborted, _Error} ->
            {not_found, {type, Type}}
    end.

get_by_node(Node, Type) ->
    {atomic, Dirs} = 
        mnesia:transaction(
          fun() -> mnesia:match_object(type_to_table(Type), {dir, '_', Node, '_', '_'}, read) end),
    Dirs.

delete(Type, Id) ->
    mnesia:transaction(
      fun() ->
              ToDelete = mnesia:match_object(type_to_table(Type), #dir{id=Id, _ = '_'}, read),
              say("deleting ~p", [ToDelete]),
              lists:foreach(
                fun(O) ->
                        mnesia:delete_object(type_to_table(Type), O, write)
                end, ToDelete)
      end).


copy_from_node(Node) ->
    lists:foreach(
      fun(Type) ->
              Dirs = rpc:call(Node, ?MODULE, get_by_node, [Node, Type]),
              %%say("adding ~p", [Dirs]),
              [add(Type, Dir) || Dir <- Dirs]
      end, get_types()).

remove_node(Node) ->
    [remove_node_refs(Node, T) || T <- get_types()].

%% Internal

type_to_table(Type) ->
    prepend(?TABLE_PREFIX, Type).

table_to_type(Table) ->
    list_to_atom(atom_to_list(Table) -- ?TABLE_PREFIX).

prepend(Prefix, Atom) when is_list(Prefix), is_atom(Atom) ->
    list_to_atom(Prefix ++ atom_to_list(Atom)).

remove_node_refs(Node, Type) ->
    mnesia:transaction(
      fun() ->
              ToDelete = mnesia:match_object(type_to_table(Type), #dir{node=Node, _ = '_'}, read),
              say("deleting ~p", [ToDelete]),
              lists:foreach(
                fun(X) ->
                        mnesia:delete_object(type_to_table(Type), X, write)
                end, ToDelete)
      end).

say(Format, Data) ->
    error_logger:info_msg("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
