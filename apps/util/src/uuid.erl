-module(uuid).

-export([new/0,
         new_sha_hex/0,
         new_sha_hex_short/0]).

-define(MILLION, 1000000).

new() ->
    {Mega, Sec, Micro} = erlang:now(),
    Num = (Mega * ?MILLION * ?MILLION) + (Sec * ?MILLION) + Micro,
    list_to_binary(integer_to_list(Num)).

new_sha_hex() ->
    hexstring(crypto:hash(sha,new())).

new_sha_hex_short() ->
    lists:sublist(new_sha_hex(), 7).

hexstring(Binary) when is_binary(Binary) ->
    lists:flatten([io_lib:format("~2.16.0b", [I]) || I <- binary_to_list(Binary)]).
