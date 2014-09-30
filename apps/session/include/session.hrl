-type type() :: atom().
-type id() :: integer().
-type tid() :: {type(), id()}.
-type sub() :: {reference(), tid()}. % subscriber/listener

-record(state, {tid :: tid(),
                created=os:timestamp(),
                listeners=[] :: [{tid(), reference()}],
                notifications=[],
                subs=[] :: [sub()],
                msgs=[] :: [term()],
                type_state :: term()}).

-compile([{parse_transform, lager_transform}]).

-define(info(Format, Args),
        lager:info("~s", [io_lib:format(Format, Args)])).

-define(warn(Format, Args),
        lager:warning("~s", [io_lib:format(Format, Args)])).

