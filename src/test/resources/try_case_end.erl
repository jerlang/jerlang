% Used by unit test.
% Minimal example to generate BEAM opcode 'try_case_end'.

-module(try_case_end).
-export([test/0]).

test() ->
    ok = try id({1,2}) of
        {_,_} -> ok
    catch
        other -> error
    end.

id(X) -> X.

