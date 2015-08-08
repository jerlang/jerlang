% List comprehension.
% Used by unit test.

-module(example5).
-export([test/0]).

test() ->
  [2*N || N <- [1,2,3]].

