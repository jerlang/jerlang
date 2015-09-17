% Used by unit test.
% Minimal example to generate the BEAM opcode bs_put_float.

-module(bs_put_float).
-export([test/1]).

test(Float) -> <<Float/float>>.

