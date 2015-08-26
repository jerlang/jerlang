% Used by unit test.

-module(is_fun).
-export([is_fun1/1,is_fun2/2]).

is_fun1(F) when is_function(F) -> true;
is_fun1(_) -> false.

is_fun2(F,A) when is_function(F,A) -> true;
is_fun2(_,_) -> false.

