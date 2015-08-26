% Used by unit test
% Minimal example to generate 'recv_mark' and 'recv_set' opcodes.

-module(recv).
-export([test/0,ref_test/1]).

test() ->
    Child = spawn(recv,ref_test,[self()]),
    R = receive
        X -> X
    end,
    Child ! {R,123},
    Response = receive
        Resp -> Resp
    end,
    Response.

ref_test(Parent) ->
    Ref = make_ref(),
    Parent ! Ref,
    receive
        {Ref,N} -> Parent ! 2*N
    end.

