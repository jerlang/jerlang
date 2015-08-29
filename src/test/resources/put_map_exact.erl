% Used by unit test.
% Minimal example to generate BEAM opcode 'put_map_exact'.

-module(put_map_exact).
-export([test/0]).

test() ->
    M0 = id(#{a=>1, b=>2}),
    M1 = M0#{a:=2,b:=3},
    M1.

id(M) -> M.

