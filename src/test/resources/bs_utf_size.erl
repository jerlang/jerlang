% Used by unit test.
% Minimal example to generate the BEAM opcodes bs_utf8_size and bs_utf16_size

-module(bs_utf_size).
-export([test8/2,test16/2,test32/2]).

test8(C, Bin) when <<C/utf8>> =:= Bin -> ok;
test8(_, _) -> error.

test16(C, Bin) when <<C/utf16>> =:= Bin -> ok;
test16(_, _) -> error.

test32(C, Bin) when <<C/utf32>> =:= Bin -> ok;
test32(_, _) -> error.

