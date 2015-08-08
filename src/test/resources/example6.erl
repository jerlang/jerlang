% List comprehension.
% Used by unit test.

-module(example6).
-export([test/0]).

test() ->
  [N || N <- [1,2,3], N rem 2 =:= 0].

