-module(session_test).

-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

session_test_() ->
    {setup,
     fun() ->
             {ok, _Started} = application:ensure_all_started(session)
     end,
     fun(_) ->
             ok = application:stop(session)
     end,
     [
      %% {spawn, ?_test(?debugVal(t_session_lifecycle()))} 
     ]}.

t_session_lifecycle() ->
    S1 = {session,1},
    S2 = {session,2},
    {ok, Pid1} = session:start(S1),
    {ok, Pid2} = session:start(S2),
    ?assertEqual(Pid1, gp:whereis(S1)),
    ?assertEqual(Pid2, gp:whereis(S2)),
    ok = session:subscribe(S1, S2),
    ?assertEqual([S2], gp:call(S1, listeners)),
    gp:cast(S1, {notify_listeners, first_post}),
    sys:get_status(Pid2), % in order to sync
    gp:cast(S1, {notify_listeners, second_post}),
    sys:get_status(Pid2), % in order to sync
    ?assertEqual([second_post, first_post], gp:call(S2, notifications)),
    ok = session:stop(S2),
    ?assertEqual([], gp:call(S1, listeners)),
    ok.

-endif.
