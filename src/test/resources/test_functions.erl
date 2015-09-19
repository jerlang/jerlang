-module(test_functions).
-export([
  test_is_atom/1,
  test_is_binary/1,
  test_is_boolean/1,
  test_is_float/1,
  test_is_integer/1,
  test_is_list/1,
  test_is_number/1,
  test_is_pid/1,
  test_is_port/1,
  test_is_tuple/1
  ]).

test_is_atom(X) when is_atom(X) -> true;
test_is_atom(_) -> false.

test_is_binary(X) when is_binary(X) -> true;
test_is_binary(_) -> false.

test_is_boolean(X) when is_boolean(X) -> true;
test_is_boolean(_) -> false.

test_is_float(X) when is_float(X) -> true;
test_is_float(_) -> false.

test_is_integer(X) when is_integer(X) -> true;
test_is_integer(_) -> false.

test_is_list(X) when is_list(X) -> true;
test_is_list(_) -> false.

test_is_number(X) when is_number(X) -> true;
test_is_number(_) -> false.

test_is_pid(X) when is_pid(X) -> true;
test_is_pid(_) -> false.

test_is_port(X) when is_port(X) -> true;
test_is_port(_) -> false.

test_is_tuple(X) when is_tuple(X) -> true;
test_is_tuple(_) -> false.

