% Used by unit test

-module(is_map).
-export([is_map1/1,is_map2/1]).

is_map1(M) ->
    erlang:is_map(M).

is_map2(M) ->
    try
        #{} = M,
        true
    catch
        error:{badmatch,_} -> false
    end.

