-record(state, {tid,
                created=os:timestamp(),
                listeners=[],
                transmitters=[],
                notifications=[],
                type_state}).

-define(info(Format, Args),
        lager:info("~s", [io_lib:format(Format, Args)])).

-define(warn(Format, Args),
        lager:warning("~s", [io_lib:format(Format, Args)])).

