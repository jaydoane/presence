-module(entity).

-compile(export_all).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

