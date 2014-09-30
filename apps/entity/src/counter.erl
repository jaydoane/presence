-module(counter).

-export([init/0, next/1, next_tid/1]).

-record(counter, {type :: atom(),
                  count :: integer()}).

-define(INCREMENT, 1).

init() ->
    mnesia:create_table(counter, [{attributes, record_info(fields, counter)}]),
    ok.

next(Type) ->
    mnesia:dirty_update_counter(counter, Type, ?INCREMENT).

next_tid(Type) ->
    {Type, next(Type)}.
