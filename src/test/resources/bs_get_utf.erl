% Used by unit test.
% Minimal example to generate BEAM opcodes:
% bs_get_utf8, bs_get_utf16, bs_get_utf32

-module(bs_get_utf).
-export([test8/1,test16/1,test32/1]).

test8(BS)  -> <<CodePoint/utf8,_/binary>>  = BS, CodePoint.
test16(BS) -> <<CodePoint/utf16,_/binary>> = BS, CodePoint.
test32(BS) -> <<CodePoint/utf32,_/binary>> = BS, CodePoint.

