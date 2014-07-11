-record(state, {tid,
                created=os:timestamp(),
                listeners=[],
                notifications=[],
                ref_tids=[],
                type_state}).

-compile([{parse_transform, lager_transform}]).

-define(info(Format, Args),
        lager:info("~s", [io_lib:format(Format, Args)])).

-define(warn(Format, Args),
        lager:warning("~s", [io_lib:format(Format, Args)])).

