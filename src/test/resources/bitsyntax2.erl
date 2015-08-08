% Used by unit test.

-module(bitsyntax2).
-export([test/0]).

test() ->
  Color = 16#112233,
  Pixel = <<Color:24>>,
  <<A0:4, A1:4, B0:4, B1:4, C0:4, C1:4>> = Pixel,
  [A0, A1, B0, B1, C0, C1].

