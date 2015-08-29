% Used by unit test.
% Minimal example to generate BEAM opcode 'put_map_assoc'.

-module(put_map_assoc).
-export([test/1]).

test(Map) ->
    #{x:="b",y:="2"} = update(Map, [{"a","1"},{"b","2"}]),
    ok.

update(Map, []) -> Map;
update(Map, [{X,Y}|Rest]) -> update(Map#{x=>X,y=>Y}, Rest).

