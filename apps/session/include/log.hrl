-compile([{parse_transform, lager_transform}]).

-define(info(Format),
        lager:info("~s", [io_lib:format(Format, [])])).

-define(info(Format, Args),
        lager:info("~s", [io_lib:format(Format, Args)])).

-define(warn(Format, Args),
        lager:warning("~s", [io_lib:format(Format, Args)])).

