package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.BitString;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a bit string.
 */
public class IsBitstr {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg = i.arg(1).toArg(proc);
        if (arg instanceof BitString) {
            return null;
        } else {
            return i.arg(0);
        }
    }

}
