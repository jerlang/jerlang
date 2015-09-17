% Used by unit test.
% Minimal example to generate the BEAM opcode bs_put_binary.

-module(bs_put_binary).
-export([test/1]).

test(Binary) -> <<1,Binary/binary>>.

