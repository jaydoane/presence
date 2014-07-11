-module(gp).

-compile(export_all).

register(Tid, Pid) ->
    global:register_name(Tid, Pid).

unregister(Tid) ->
    global:unregister_name(Tid).

whereis(Tid) ->
    global:whereis_name(Tid).

status(Tid) ->
    case ?MODULE:whereis(Tid) of
        undefined ->
            undefined;
        Pid ->
            sys:get_status(Pid)
    end.

call(Tid, Msg) ->
    try
        gen_server:call(?MODULE:whereis(Tid), Msg)
    catch
        _:Error ->
            error_logger:warning_msg("gp:call ~p ~p ~p", [Tid, Msg, Error]),
            {error, Error}
    end.

cast(Tid, Msg) ->
    gen_server:cast(?MODULE:whereis(Tid), Msg).

notify(Tid, Msg) ->
    cast(Tid, {notify, Msg}).
