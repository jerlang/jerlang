% Used by unit test.

-module(is_ref).
-export([test_is_ref/1,test_get_ref/0]).

test_is_ref(X) when is_reference(X) -> true;
test_is_ref(_) -> false.

test_get_ref() -> erlang:make_ref().

