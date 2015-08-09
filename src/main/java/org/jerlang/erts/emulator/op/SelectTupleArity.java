package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Check the arity of the tuple Tuple and jump to the corresponding
 * destination label, if no arity matches, jump to FailLabel.
 */
public class SelectTupleArity {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("Not implemented: " + i);
    }

}
