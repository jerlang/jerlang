package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a binary.
 */
public class IsBinary {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg1 = i.arg(1).toArg(proc);
        if (arg1 instanceof Binary) {
            return null;
        } else {
            return i.arg(0);
        }
    }

}
