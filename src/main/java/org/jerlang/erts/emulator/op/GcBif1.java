package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call the bif Bif with the argument Arg, and store the result in Reg.
 * On failure jump to Lbl.
 * Do a garbage collection if necessary to allocate space on the heap
 * for the result (saving Live number of X registers).
 */
public class GcBif1 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("Not implemented: " + i);
    }

}
