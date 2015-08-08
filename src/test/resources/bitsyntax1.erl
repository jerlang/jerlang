% Used by unit test.

-module(bitsyntax1).
-export([test/0]).

test() ->
  Color = 16#112233,
  Pixel = <<Color:24>>,
  Pixel.

