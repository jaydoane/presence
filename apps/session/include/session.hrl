-type type() :: atom().
-type id() :: integer().
-type tid() :: {type(), id()}.

-record(state, {tid :: tid(),
                created=os:timestamp(),
                listeners=[] :: [{tid(), reference()}],
                notifications=[],
                type_state}).

-compile([{parse_transform, lager_transform}]).

-define(info(Format, Args),
        lager:info("~s", [io_lib:format(Format, Args)])).

-define(warn(Format, Args),
        lager:warning("~s", [io_lib:format(Format, Args)])).

