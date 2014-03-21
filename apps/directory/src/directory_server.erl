-module(directory_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Behaviour

init([]) ->
    log:info("setting state to ~p", [[]]),
    net_kernel:monitor_nodes(true),
    {ok, []}.

handle_call('__not_implemented', _From, State) ->
    {noreply, State}.

handle_cast('__not_implemented', State) ->
    {noreply, State}.

handle_info(Info, State) ->
    say("info ~p, ~p.", [Info, State]),
    case Info of
        {nodeup, Node} ->
            nodeup(Node);
        {nodedown, Node} ->
            nodedown(Node)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    say("terminate ~p, ~p", [_Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
    {ok, State}.

%% Internal

nodeup(Node) ->
    say("copying directory data from ~p", [Node]),
    Copied = directory:copy_from_node(Node),
    say("copied: ~p", [Copied]).

nodedown(Node) ->
    say("removing directory data for ~p", [Node]),
    Removed = directory:remove_node(Node),
    say("removed ~p", [Removed]).


say(Format, Data) ->
    error_logger:info_msg("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
