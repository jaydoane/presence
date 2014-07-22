-module(ti_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen, socket}).

start_link(Listen) ->
    gen_server:start_link(?MODULE, [Listen], []).

init([Listen]) ->
    {ok, #state{listen = Listen}, _Timeout=0}. % immediately handle_info(timeout

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};
handle_info({tcp_closed, Socket}, State) ->
    log:info("tcp_closed ~p, stopping", [Socket]),
    {stop, normal, State};
handle_info(timeout, #state{listen = Listen} = State) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    ti_sup:start_child(),
    {noreply, State#state{socket = Socket}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

handle_data(Socket, RawData, State) ->
    try
        log:info("RawData: ~p", [RawData]),
        gen_tcp:send(Socket, io_lib:fwrite("OK:~p.~n", [RawData]))
    catch
        _Class:Err ->
            gen_tcp:send(Socket, io_lib:fwrite("ERROR:~p.~n", [Err]))
    end,
    State.
%% handle_data(Socket, RawData, State) ->
%%     try
%%         {Function, RawArgList} =
%%             lists:splitwith(fun (C) -> C =/= $[ end, RawData),
%%         log:info("Function: ~p, RawArgList: ~p", [Function, RawArgList]),
%%         {ok, Toks, _Line} = erl_scan:string(RawArgList ++ ".", 1),
%%         {ok, Args} = erl_parse:parse_term(Toks),
%%         Result = apply(simple_cache, list_to_atom(Function), Args),
%%         gen_tcp:send(Socket, io_lib:fwrite("OK:~p.~n", [Result]))
%%     catch
%%         _Class:Err ->
%%             gen_tcp:send(Socket, io_lib:fwrite("ERROR:~p.~n", [Err]))
%%     end,
%%     State.
