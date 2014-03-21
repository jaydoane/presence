-module(session_test).

-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

session_test_() ->
    {setup,
     fun() ->
             application:start(session)
             ,application:start(lager)
     end,
     fun(_) ->
             application:stop(session)
             ,application:stop(lager)
     end,
     [
      {spawn, ?_test(?debugVal(t_session_lifecycle()))} 
     ]}.

t_session_lifecycle() ->
    S1 = {session,1},
    S2 = {session,2},
    {ok, Pid1} = session_sup:start_child(S1),
    {ok, Pid2} = session_sup:start_child(S2),
    ?assertEqual(Pid1, gp:whereis(S1)),
    ?assertEqual(Pid2, gp:whereis(S2)),
    session:subscribe(S1, S2),
    ?assertEqual([S2], gp:call(S1, listeners)),
    ?assertEqual([S1], gp:call(S2, transmitters)),
    gp:cast(S1, {notify_listeners, first_post}),
    sys:get_status(Pid2), % in order to sync
    gp:cast(S1, {notify_listeners, second_post}),
    sys:get_status(Pid2), % in order to sync
    ?assertEqual([second_post, first_post], gp:call(S2, notifications)),
    ok = session_sup:stop_child(Pid2),
    sys:get_status(Pid1), % in order to sync
    ?assertEqual([], gp:call(S1, listeners)),
    ok.

-endif.
