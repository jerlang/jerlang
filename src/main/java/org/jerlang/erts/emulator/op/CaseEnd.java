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
 * Example:
 * ** exception error: no case clause matching {1,2}
 *    in function  case_end:test/0 (case_end.erl, line 8)
 */
public class CaseEnd {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error(Tuple.of(Atom.of("case_clause"), i.arg(0).toArg(proc)));
    }

}
