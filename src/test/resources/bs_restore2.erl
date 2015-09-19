% Part of JErlang test suite.
% Minimal example to generate BEAM opcode 'bs_restore2'.

-module(bs_restore2).
-export([test/1]).

test(<<X>>) when X == 0 ->
    zero;
test(<<0:4,_:4>>) ->
    halfbyte;
test(<<_>>) ->
    fullbyte.

