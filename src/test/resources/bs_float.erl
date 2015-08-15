% Used by unit test.

-module(bs_float).
-export([test/0]).

test() ->
  test1(<<1.23/float>>).

test1(<<F/float>>) -> F.

