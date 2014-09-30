-module(test).

-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hail_test_() ->
    {setup,
     fun() ->
             ok = mnesia:start(),
             {ok, _Started} = application:ensure_all_started(entity) end,
     fun(_) ->
             ok = application:stop(entity),
             ok = application:stop(lager),
             ok = application:stop(goldrush),
             mnesia:stop(),
             ok
     end,
     [
      {spawn, ?_test(?debugVal(t_order_cancel()))} 
     ]}.
    
-endif.

t_order_cancel() ->
    {ok, RiderTid} = rider:create([{name,"A Rider"}]),
    observing = gen_entity:state_name(RiderTid),
    {ok, OrderTid} = rider:order(RiderTid, [{address, "314 16th St"}]),
    ordering = gen_entity:state_name(RiderTid),
    processing = gen_entity:state_name(OrderTid),
    [{RiderTid, _Pid, _Ref}] = gen_entity:subs(OrderTid),
    {ok, canceled} = order:cancel(OrderTid, normal),
    observing = gen_entity:state_name(RiderTid),
    [] = gen_entity:subs(OrderTid),
    ok.
