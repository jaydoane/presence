%%% @doc
%%% Test suite for directory application

-module(directory_SUITE).

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

%%% External exports
-compile(export_all).

-define(TOP_DIR, "presence/").
-define(NODES, ['directory-ct-1', 'directory-ct-2']).
-define(ERL_FLAGS, "-kernel dist_auto_connect once").
-define(DISCONNECT_TIME, 4000).
-define(UNSPLIT_TIMEOUT, 5000).

absolute_top(Path) ->
    string:substr(Path, 1, string:str(Path, ?TOP_DIR)-1) ++ ?TOP_DIR.

all() ->
    [directory_cluster].

init_per_suite(Conf) ->
    ct:print("init_per_suite Conf: ~p", [Conf]),
    {ok, Cwd} = file:get_cwd(),
    AbdTopDir = absolute_top(Cwd),
    ct:print("top dir: ~p", [AbdTopDir]),
    Nodes = ct:get_config(nodes, ?NODES),
    DisconnectTime = ct:get_config(disconnect_time, ?DISCONNECT_TIME),
    UnsplitTimeout = ct:get_config(unsplit_timeout, ?UNSPLIT_TIMEOUT),
    Host = get_host(),
    EbinDirs = ["apps/directory/ebin"],
    ErlFlags = 
        lists:flatten(
          [?ERL_FLAGS,
           get_path_flags(),
           " -pa ", filename:absname(filename:dirname(code:which(?MODULE))),
           " -pa ", [AbdTopDir ++ Path ++ " " || Path <- EbinDirs],
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
    [{disconnect_time, DisconnectTime},
     {unsplit_timeout, UnsplitTimeout},
     {enable_builtin_hooks,false},
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

directory_cluster() ->
    [{userdata, [{doc, "Tests connection and disconnection of 2 nodes"}]}].

directory_cluster(Conf)->
    Type = passenger,
    Nodes = [Node1, Node2] = get_conf(nodes, Conf),
    ct:print("Nodes = ~p~n",[Nodes]),
    %% Node1 = hd(Nodes),
    %% Node2 = hd(tl(Nodes)),

    CreateDirectory = rpc:multicall(Nodes, directory_server, create_directory, [Type]),
    ct:print("create_directory ~p", [CreateDirectory]),

    rpc:call(Node1, directory_server, add, [Type, 1]),
    rpc:call(Node2, directory_server, add, [Type, 2]),

    pong = rpc:call(Node1, net_adm, ping, [Node2]),
    ct:print("~p ping ~p successful.", [Node1, Node2]),
    %% R1 = rpc:call(Node1, erlang, nodes, []),
    %% R2 = rpc:call(Node2, erlang, nodes, []),
    %% ct:print("nodes() on Node1: ~p nodes() on Node2: ~p", [R1, R2]),

    %% rpc:call(Node1, directory_server, nodeup, [Node2]),
    %% rpc:call(Node2, directory_server, nodeup, [Node1]),
    Result = rpc:call(Node2, directory_server, get, [Type, 1]),
    ct:print("result ~p", [Result]),

    %% 0 = length(connected_users(Node1)),
    %% 0 = length(connected_users(Node2)),

    %% UID = 123,
    %% {ok, _SID} = rpc:call(Node1, liveride, register_connection, 
    %%                      [UID, <<"default">>, tcp_session, self(), null]),
    %% 1 = length(connected_users(Node1)),
    %% 1 = length(connected_users(Node2)),
    %% rpc:call(Node1, liveride, disconnect, [UID, "quit"]),
    %% 0 = length(connected_users(Node1)),
    %% 0 = length(connected_users(Node2)),
    ct:print("done").


%% connected_users(Node) ->
%%     rpc:call(Node, mnesia, dirty_match_object, [#connected_user{_='_'}]).

get_conf(Key, Conf)->
    proplists:get_value(Key, Conf).


init_nodes(Nodes)->
    ct:print("init nodes ~p", [Nodes]),
    Init = 
        fun(Node)->
                rpc:call(Node, application, start, [sasl]),
                rpc:call(Node, mnesia, start, []),
                rpc:call(Node, application, start, [directory])
        end,
    lists:foreach(Init, Nodes).


terminate_nodes(Nodes)->
    Terminate = 
        fun(Node)->
                rpc:call(Node, application, stop, [directory]),
                rpc:call(Node, mnesia, stop,[]),
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
