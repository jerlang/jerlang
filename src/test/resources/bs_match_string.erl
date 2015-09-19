% JErlang test suite
% Minimal example to generate the BEAM opcode 'bs_match_string'

-module(bs_match_string).
-export([test/1]).

test(Binary) ->
    case Binary of
    <<16:8,Tail/binary>> -> Tail;
    _ -> error
    end.

