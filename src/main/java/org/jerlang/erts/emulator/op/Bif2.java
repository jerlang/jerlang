package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call the bif Bif with the arguments Arg1 and Arg2,
 * and store the result in Reg.
 * On failure jump to Lbl.
 *
 * Arguments:
 * 1. Lbl
 * 2. Bif
 * 3. Arg1
 * 4. Arg2
 * 5. Reg
 */
public class Bif2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("not implemented yet: " + i);
    }

}
