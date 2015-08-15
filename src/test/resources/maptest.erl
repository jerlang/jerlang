% Used by unit test.
% Maps.

-module(maptest).
-export([test/0]).

test() ->
    M = #{a => 1, b => 2},
    #{a := 1, b := _} = M,
    N = M#{ c => 3, b := 4},
    {ok, Value} = maps:find(b, N),
    Value.

