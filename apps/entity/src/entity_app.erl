-module(entity_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = counter:init(), % replace with better fake persistence
    entity_sup:start_link().

stop(_State) ->
    ok.
