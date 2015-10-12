% Used by unit test.
% Minimal example to produce a big integer.

-module(bignum).
-export([test/0]).

test() ->
    BigNum = id(1 bsl 256),
    BigNum.

id(N) -> N.
 
