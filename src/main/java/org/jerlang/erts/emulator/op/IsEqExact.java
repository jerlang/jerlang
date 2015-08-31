package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * is equal
 *
 * Arguments:
 * 1. Lbl
 * 2. Arg1
 * 3. Arg2
 *
 * Compare two terms and jump to Lbl if Arg1 is not exactly equal to Arg2.
 */
public class IsEqExact {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term lbl = i.arg(0);
        Term arg1 = i.arg(1).toArg(proc);
        Term arg2 = i.arg(2).toArg(proc);
        if (arg1.equals(arg2)) {
            return null;
        } else {
            return lbl;
        }
    }

}
