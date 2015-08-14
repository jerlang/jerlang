% Used by unit test.

-module(tuple).
-export([test/1]).

test(T) ->
  {A, _} = T,
  {1, A}.

