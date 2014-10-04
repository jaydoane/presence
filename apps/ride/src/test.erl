-module(test).

-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hail_test_() ->
    {setup,
     fun() ->
             error_logger:tty(false),
             ok = mnesia:start(),
             {ok, _Started} = application:ensure_all_started(entity),
             lager:set_loglevel(lager_console_backend, error),
             ok end,
     fun(_) ->
             ok = application:stop(entity),
             ok = application:stop(lager),
             ok = application:stop(goldrush),
             mnesia:stop(),
             error_logger:tty(true),
             ok end,
     [
      {spawn, ?_test(?debugVal(t_order_cancel()))} 
      ,{spawn, ?_test(?debugVal(t_hail_timeout()))} 
     ]}.
    
-endif.

create_rider_and_order() ->
    {ok, Rider} = rider:create([{name,"A Rider"}]),
    observing = gen_entity:state(Rider),
    {ok, Order} = rider:order(Rider, [{address, "314 16th St"}]),
    ordering = gen_entity:state(Rider),
    processing = gen_entity:state(Order),
    [{Rider, _, _}] = gen_entity:subs(Order),
    {Rider, Order}.

t_order_cancel() ->
    {Rider, Order} = create_rider_and_order(),
    {ok, canceled} = order:cancel(Order, normal),
    observing = gen_entity:state(Rider),
    [] = gen_entity:subs(Order),
    ok.

t_hail_timeout() ->
    {Rider, Order} = create_rider_and_order(),
    {ok, Driver} = driver:create([{name, "A Driver"}]),
    available = gen_entity:state(Driver),
    {ok, Hail} = driver:hail(Driver, Order, [{timeout_ms,9}]),
    in_hail = gen_entity:state(Driver),
    [] = gen_entity:subs(Driver),
    hailing = gen_entity:state(Hail),
    [_S1, _S2] = gen_entity:subs(Hail),
    timer:sleep(10),
    available = gen_entity:state(Driver),
    timed_out = gen_entity:state(Hail),
    [] = gen_entity:subs(Hail),
    ordering = gen_entity:state(Rider),
    ok.

