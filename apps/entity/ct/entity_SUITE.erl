%%% @doc
%%% Test suite for gen_entity

-module(entity_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(EBIN_DIRS, ["apps/*/ebin", "deps/*/ebin"]).

-define(TOP_DIR, "presence/").
-define(NODES, ['presence-ct-1', 'presence-ct-2']).
-define(ERL_FLAGS, "-kernel dist_auto_connect once").


absolute_top(Path) ->
    string:substr(Path, 1, string:str(Path, ?TOP_DIR)-1) ++ ?TOP_DIR.

all() ->
    [name_conflict].

name_conflict() ->
    [{userdata, [{doc, "By default, gen_entity prefers the newest instance "
                  "in name conflicts on net joins"}]}].

name_conflict(Conf)->
    Nodes = [Node1, Node2] = get_conf(nodes, Conf),
    %% ct:print("Nodes: ~p",[Nodes]),

    DisconnectResult = [rpc:multicall(Nodes, erlang, disconnect_node, [Node], 1000) || Node <- Nodes],
    ct:print("DisconnectResult ~p", [DisconnectResult]),

    %% ct:print("nodes: ~p", [rpc:call(Node1, erlang, nodes, [])]),
    Tid = {example,1},

    {ok, Pid1} = rpc:call(Node1, example, start, [Tid,[]]),
    ct:print("Pid1 ~p", [Pid1]),

    timer:sleep(10),

    {ok, Pid2} = rpc:call(Node2, example, start, [Tid,[]]),
    ct:print("Pid2 ~p", [Pid2]),

    ConnectResult = [rpc:multicall(Nodes, net_kernel, connect_node, [N], 1000) || N <- Nodes],
    ct:print("ConnectResult ~p", [ConnectResult]),

    pong = rpc:call(Node1, net_adm, ping, [Node2]),
    ct:print("~p ping ~p successful", [Node1, Node2]),

    Pid2 = rpc:call(Node2, gp, whereis, [Tid]),
    %% ct:print("last one created survived ~p", [Pid2]),

    %% timer:sleep(2000),
    %% false = rpc:call(Node1, erlang, is_process_alive, [Pid1]),
    ok.


init_per_suite(Conf) ->
    ct:print("init_per_suite Conf: ~p", [Conf]),
    Nodes = ct:get_config(nodes, ?NODES),
    ErlFlags = erl_flags(),
    ct:print("ErlFlags = ~p~n", [ErlFlags]),
    NodeNames = lists:map(fun(Node) -> start_node(Node, ErlFlags) end, Nodes),
    [{enable_builtin_hooks,false}, {nodes, NodeNames}|Conf].

start_node(Node, ErlFlags) ->
    Host = get_host(),
    ct:print("starting node ~p, on host ~p ~n",[Node, Host]),
    {ok, NodeName} = ct_slave:start(Host, Node, [{erl_flags, ErlFlags}, {monitor_master, true}]),
    NodeName.

erl_flags() ->
    {ok, Cwd} = file:get_cwd(),
    ct:print("cwd: ~p", [Cwd]),
    AbsTopDir = absolute_top(Cwd),
    ct:print("top dir: ~p", [AbsTopDir]),
    lists:flatten(
      [?ERL_FLAGS,
       get_path_flags(),
       " -pa ", filename:absname(filename:dirname(code:which(?MODULE))),
       " -pa ", [AbsTopDir ++ Path ++ " " || Path <- ?EBIN_DIRS],
       " -boot start_sasl"
      ]).
    
end_per_suite(_Conf) ->
    Nodes = ct:get_config(nodes,?NODES),
    Host = get_host(),
    StopNode = fun(Node)->
                       {ok, _NodeName} = ct_slave:stop(Host, Node)
               end,
    lists:map(StopNode, Nodes),
    ok.

init_per_testcase(Case, Conf) ->
    ct:print("Test case ~p started", [Case]),
    init_nodes(get_conf(nodes, Conf)),
    Conf.

end_per_testcase(Case, Conf) ->
    ct:print("Test case ~p finished", [Case]),
    terminate_nodes(get_conf(nodes, Conf)),
    Conf.

get_conf(Key, Conf)->
    proplists:get_value(Key, Conf).

init_nodes(Nodes)->
    ct:print("init nodes ~p", [Nodes]),
    Init = 
        fun(Node)->
                rpc:call(Node, application, start, [sasl]),
                rpc:call(Node, lager, start, []),
                rpc:call(Node, mnesia, start, []),
                rpc:call(Node, entity, start, [])
        end,
    lists:foreach(Init, Nodes).


terminate_nodes(Nodes)->
    Terminate = 
        fun(Node)->
                rpc:call(Node, entity, stop, []),
                rpc:call(Node, mnesia, stop,[]),
                rpc:call(Node, lager, stop,[]),
                rpc:call(Node, application, stop, [sasl])
        end,
    lists:foreach(Terminate, Nodes).


get_host()->
    [_, H] = re:split(atom_to_list(node()),"@",[{return,list}]),
    list_to_atom(H).

get_path_flags() ->
    [ [[" -",atom_to_list(K)," ",D] || D <- V]
      || {K,V} <- init:get_arguments(),
     K == pa orelse K == pz].
