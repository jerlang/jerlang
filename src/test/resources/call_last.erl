% Used by unit test.
% Minimal example that generates a 'call_last' BEAM opcode.

-module(call_last).
-export([test/1]).

test(A) ->
    F = fun(B) ->
        B - 1
    end,
    local(A,F).

local(X,F) when X > 0 ->
    X + local(F(X - 1),F);
local(0,_) ->
    1.

