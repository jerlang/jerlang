% JErlang test suite.
% Minimal example to generate the BEAM opcode bs_init_writable.

-module(bs_init_writable).
-export([test/0]).

test() ->
    << <<(X+32)>> || <<X>> <= <<"ABC">> >>.

