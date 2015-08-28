% Used by unit test.
% Minimal example that generates a 'call_fun' opcode.

-module(call_fun).
-export([test/1]).

test(A) ->
    F = fun(B) ->
        A + B
    end,
    F(A).

