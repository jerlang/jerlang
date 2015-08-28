% Used by unit test.
% Minimal example to generate BEAM opcode 'apply_last'.

-module(apply_last).
-export([test/1]).

test(Mod) ->
    Mod:module_info(module).

