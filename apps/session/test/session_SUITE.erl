%%% @doc
%%% Test suite for session application

-module(session_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%% External exports
-compile(export_all).

-define(TOP_DIR, "presence/").
-define(NODES, ['session-ct-1', 'session-ct-2']).
-define(ERL_FLAGS, "-kernel dist_auto_connect once").

absolute_top(Path) ->
    string:substr(Path, 1, string:str(Path, ?TOP_DIR)-1) ++ ?TOP_DIR.

all() ->
    [session_cluster].

init_per_suite(Conf) ->
    ct:print("init_per_suite Conf: ~p", [Conf]),
    {ok, Cwd} = file:get_cwd(),
    AbsTopDir = absolute_top(Cwd),
    ct:print("top dir: ~p", [AbsTopDir]),
    Nodes = ct:get_config(nodes, ?NODES),
    Host = get_host(),
    EbinDirs = ["apps/*/ebin", "deps/*/ebin"],
    ErlFlags = 
        lists:flatten(
          [?ERL_FLAGS,
           get_path_flags(),
           " -pa ", filename:absname(filename:dirname(code:which(?MODULE))),
           " -pa ", [AbsTopDir ++ Path ++ " " || Path <- EbinDirs],
           " -boot start_sasl"
          ]),
    ct:print("ErlFlags = ~p~n", [ErlFlags]),
    StartNode = 
        fun(Node)->
                ct:print("starting node ~p, on host ~p ~n",[Node, Host]),
                {ok, NodeName} = ct_slave:start(Host, Node,
                                                [{erl_flags, ErlFlags},
                                                 {monitor_master, true}]),
                NodeName
        end,
    NodeNames = lists:map(StartNode, Nodes),
    [{enable_builtin_hooks,false},
     {nodes, NodeNames}|Conf].

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

session_cluster() ->
    [{userdata, [{doc, "Tests connection and disconnection of 2 nodes"}]}].

session_cluster(Conf)->
    Nodes = [Node1, Node2] = get_conf(nodes, Conf),
    ct:print("Nodes: ~p",[Nodes]),

    R1 = rpc:call(Node1, session, start, [{driver,1}]),
    R2 = rpc:call(Node2, session, start, [{driver,2}]),
    ct:print("R1: ~p, R2: ~p", [R1, R2]),

    pong = rpc:call(Node1, net_adm, ping, [Node2]),
    ct:print("~p ping ~p successful", [Node1, Node2]),

    Pid = rpc:call(Node2, gp, whereis, [{driver,1}]),
    true =:= is_pid(Pid),
    ct:print("done").


get_conf(Key, Conf)->
    proplists:get_value(Key, Conf).


init_nodes(Nodes)->
    ct:print("init nodes ~p", [Nodes]),
    Init = 
        fun(Node)->
                rpc:call(Node, application, start, [sasl]),
                rpc:call(Node, lager, start, []),
                rpc:call(Node, mnesia, start, []),
                rpc:call(Node, session, start, [])
        end,
    lists:foreach(Init, Nodes).


terminate_nodes(Nodes)->
    Terminate = 
        fun(Node)->
                rpc:call(Node, session, stop, []),
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
