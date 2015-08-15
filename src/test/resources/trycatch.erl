% Used by unit test.

-module(trycatch).
-export([test/0]).

test() ->
  try something()
  catch
    _:_ -> exception
  end.

something() -> throw({something}).

