% Used by unit test.
% Generates bif2 opcode.

-module(is_record).
-export([test/2]).

test(Term, RecordTag) ->
    is_record(Term, RecordTag).

