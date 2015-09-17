% Used by unit test
% Minimal example to generate the BEAM opcode 'gc_bif3'.

-module(gc_bif3).
-export([test/3]).

test(Term, RecordTag, Size) ->
    binary_part(Term, RecordTag, Size).

