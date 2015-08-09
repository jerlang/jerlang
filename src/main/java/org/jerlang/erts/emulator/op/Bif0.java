package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call the bif Bif and store the result in Reg.
 *
 * Arguments:
 * 1. Bif
 * 2. Reg
 */
public class Bif0 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("not implemented yet: " + i);
    }

}
