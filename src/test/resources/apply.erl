% Used by unit test.
% Minimal example to generate BEAM opcode 'apply'.

-module(apply).
-export([test/1]).

test(Mod) ->
    Exports = Mod:module_info(module),
    {module,Exports}.

