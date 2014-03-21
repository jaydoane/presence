-module(app).

%% @doc Allow applications to be started from command line: -s app start application-name

-export([start/1]).

start([App]) ->
   application:start(App).
