% Used by unit test.
% Minimal example to generate BEAM opcode 'if_end'.

-module(if_end).
-export([test/1]).

test(N) ->
    try if N =:= 1 -> ok end of
        ok -> ok
    catch
        _:_ -> error
    end.

