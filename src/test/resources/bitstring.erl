% Used in unit test.
% invoke: bitstring:test(<<433:16,3:3>>).

-module(bitstring).
-export([test/1]).

test(X) ->
    S = byte_size(X),
    {X,S}.

