% Used by unit test.
% Minimal example to generate BEAM opcode 'catch'.

-module(test_catch).
-export([test/0]).

test() ->
    F = fun(X) -> X + 1 end,
    {'EXIT', _} = (catch F(x)),
    ok.

