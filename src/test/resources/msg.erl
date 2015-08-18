-module(msg).
-export([test/0, loop/1]).

test() ->
    Pid = spawn(msg,loop,[self()]),
    erlang:send_after(500, Pid, {kill,self()}),
    X = receive
        Response ->
            Response
    end,
    X.

loop(Parent) ->
    receive
        {kill,Sender} ->
            Sender ! killed;
        _ ->
            loop(Parent)
    after
        1000 ->
            Parent ! timeout
    end.

