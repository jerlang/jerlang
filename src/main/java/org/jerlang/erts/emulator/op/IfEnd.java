package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Indicates that no branch has matched.
 * Generates an error.
 *
 * Example:
 * ** exception error: no true branch found when evaluating an if expression
 *    in function  if_end:test/1 (if_end.erl, line 8)
 */
public class IfEnd {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error(Tuple.of(Atom.of("if_clause")));
    }

}
