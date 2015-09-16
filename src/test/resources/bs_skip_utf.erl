% Used by unit tests.
% Minimal example to produce the BEAM opcodes:
% bs_skip_utf8, bs_skip_utf16, bs_skip_utf32

-module(bs_skip_utf).
-export([test8/1,test16/1,test32/1]).

test8(BS)  -> <<_/utf8,R/binary>>  = BS, R.
test16(BS) -> <<_/utf16,R/binary>> = BS, R.
test32(BS) -> <<_/utf32,R/binary>> = BS, R.

