%% A gen_server that counts. OTP's own gen_server runs it; nothing here is a
%% JErlang rewrite.
-module(counter).
-behaviour(gen_server).

-export([start/0, increment/1, value/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start() ->
    gen_server:start({local, counter}, ?MODULE, 0, []).

increment(Server) ->
    gen_server:cast(Server, increment).

value(Server) ->
    gen_server:call(Server, value).

stop(Server) ->
    gen_server:stop(Server).

init(Count) ->
    {ok, Count}.

handle_call(value, _From, Count) ->
    {reply, Count, Count}.

handle_cast(increment, Count) ->
    {noreply, Count + 1}.
