% Factorial.
% Used by unit test.
%
% Example taken from:
% http://learnyousomeerlang.com/recursion

-module(example2).
-export([fac/1]).

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N-1).

