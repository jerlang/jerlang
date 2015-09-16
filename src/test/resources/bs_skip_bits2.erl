% Used by unit test.
% Minimal example to generate BEAM opcode bs_skip_bits2

-module(bs_skip_bits2).
-export([test/1]).

test(BS) ->
    <<_:8, A:8>> = BS,
    A.

