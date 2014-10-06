-compile([{parse_transform, lager_transform}]).

-define(trace(Args),
        lager:info("~s", [io_lib:format(string:strip(lists:concat(["~p " || _A <- Args])), Args)])).

-define(debug(Format, Args),
        lager:debug("~s", [io_lib:format(Format, Args)])).

-define(info(Format),
        lager:info("~s", [io_lib:format(Format, [])])).

-define(info(Format, Args),
        lager:info("~s", [io_lib:format(Format, Args)])).

-define(warn(Format, Args),
        lager:warning("~s", [io_lib:format(Format, Args)])).

