% JErlang test suite
% Minimal example to generate the BEAM opcode 'bs_append'.

-module(bs_append).
-export([test/1]).

test(Binary) ->
    <<Binary/binary,0>>.

