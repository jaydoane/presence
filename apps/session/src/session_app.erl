-module(session_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% case session_sup:start_link() of
    %%     {ok, Pid} ->
    %%         {ok, Pid};
    %%     Other ->
    %%         {error, Other}
    %% end.
    ok = counter:init(), % replace with better fake persistence
    domain_sup:start_link(). % FIXME

stop(_State) ->
    ok.
