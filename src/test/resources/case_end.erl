% Used by unit test.
% Minimal example to generate BEAM opcode 'case_end'.

-module(case_end).
-export([test/0]).

test() ->
    try test_case() of
        Result -> Result
    catch
        _:_ -> error
    end.

test_case() ->
    ok = case id({1,2}) of
        {ok,_} -> ok
    end.

id(X) -> X.

