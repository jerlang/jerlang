package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Compare two terms and jump to Lbl if Arg1 is not (numerically)
 * equal to Arg2.
 *
 * Example:
 * {is_eq,{f,3},{x,0},0}
 */
public class IsEq {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        // TODO: Add test for Float/Integer

        Term a = i.arg(1).toArg(proc);
        Term b = i.arg(2).toArg(proc);

        if (a.equals(b)) {
            return null;
        } else {
            return i.arg(0);
        }
    }

}
