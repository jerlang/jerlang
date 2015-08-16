% Used in unit test.

-module(select).
-export([test/1]).

test(X) ->
  case X of
    % select_val with integers
    1 -> "one";
    2 -> "two";
    "one" -> 1;
    "two" -> 2;
    % select_val with atoms
    one -> 1;
    two -> 2;
    % select_tuple_arity
    {a} -> 1;
    {a,b} -> 2;
    _ -> "NaN"
  end.

