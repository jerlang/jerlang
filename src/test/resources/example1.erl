% Example of a case statement.
% Used by unit test.

-module(example1).
-export([test/1]).

test(X) ->
  case X >= 0 of
    true -> positive;
    false -> negative
  end.

