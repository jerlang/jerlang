package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Test the arity of (the tuple in) Arg1 and jump
 * to Lbl if it is not equal to Arity.
 *
 * Arguments:
 * 1. Label
 * 2. Arg1
 * 3. Arity
 */
public class TestArity {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Tuple tuple = i.arg(1).toArg(proc).toTuple();
        Integer arity = i.arg(2).toInteger();
        if (tuple.arity() == arity.toInt()) {
            return null;
        } else {
            return i.arg(0);
        }
    }

}
