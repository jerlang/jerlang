% JErlang test suite.

-module(base64test).
-export([test1/0,test2/0]).

test1() ->
    Input = <<0,1,2,3,4,5,6,7,8,9>>,
    Encoded = base64:encode(Input),
    Decoded = base64:decode(Encoded),
    Input = Decoded,
    Encoded.

test2() ->
    Input = <<0,1,2,3,4,5,6,7,8,9>>,
    Encoded = base64:encode_to_string(Input),
    Decoded = base64:decode(Encoded),
    Input = Decoded,
    Encoded.

