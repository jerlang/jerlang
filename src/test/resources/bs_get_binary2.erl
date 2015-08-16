-module(bs_get_binary2).
-export([test/0]).

test() ->
  <<A,B/bitstring>> = <<1,17,42:12>>,
  {A,B}.

