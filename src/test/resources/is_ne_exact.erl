% Used by unit test.
% Minimal example that generates an 'is_ne_exact' opcode.

-module(is_ne_exact).
-export([test/1]).

test(X) when X =/= 0 ->
    true;
test(_) ->
    false.

