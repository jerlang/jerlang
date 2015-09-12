% Used by unit test
% Minimal example for the ++ operator

-module(plusplus).
-export([test/1]).

test(L) -> L ++ [0].

