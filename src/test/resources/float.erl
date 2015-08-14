% Example of float functions.
% Used by unit test.

-module(float).
-export([test/1]).

test(X) ->
  A = X + 1.5,
  B = A * 1.5,
  C = B / 1.5,
  D = -C,
  E = D - 1.5,
  E.

