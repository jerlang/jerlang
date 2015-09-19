% Part of JErlang test suite.
% Minimal example to generate BEAM opcode 'bs_save2'.

-module(bs_save2).
-export([test/1]).

test(<<X>>) when X == 0 ->
    a;
test(<<_,_:2/binary>>) ->
    b;
test(<<_>>) ->
    c.

