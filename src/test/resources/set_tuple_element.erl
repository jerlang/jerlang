% Used by unit test.
% A minimal example that generates a 'set_tuple_element' opcode.

-module(set_tuple_element).
-export([test/1]).

test(T) ->
    {A,B} = T,
    T1 = {A,2,B,4},
    T2 = setelement(3, T1, 0),
    T3 = setelement(2, T2, 0),
    T3.

