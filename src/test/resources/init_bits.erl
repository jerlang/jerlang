% Used by unit test.
% Minimal example to generate BEAM opcode 'bs_init_bits'

-module(init_bits).
-export([test/1]).

test(X) ->
    <<(X+1):3>>.

