% Used by unit test.
% Minimal example to generate BEAM opcode 'bs_put_string'.

-module(bs_put_string).
-export([test/0]).

test() ->
    <<1:99,4,3,2,1:1>> = list_to_bitstring(bitstring_to_list(<<1:99,4,3,2,1:1>>)),
    ok.

