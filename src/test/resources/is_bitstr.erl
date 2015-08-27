% Used by unit test

-module(is_bitstr).
-export([test/1,test2/1]).

test(BitString) ->
    erlang:is_bitstr(BitString).

test2(X) when is_bitstring(X) -> true;
test2(_) -> false.

