% Stack test.
% Used by unit test.

-module(example3).
-export([stacktest/1]).

stacktest(N)  -> stacktest2(N).

stacktest2(N) -> 2 * N.

