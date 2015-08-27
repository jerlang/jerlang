% Used by unit test.
% Generates 'is_eq' and 'is_ne' opcodes.

-module(is_eq).
-export([eq/1,ne/1]).

eq(N) when N == 0 -> true;
eq(_) -> false.

ne(N) when N /= 0 -> true;
ne(_) -> false.

