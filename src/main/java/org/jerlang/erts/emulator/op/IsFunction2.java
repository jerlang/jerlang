package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Fun;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a
 * function of arity Arity.
 *
 * Arguments:
 * 1. Lbl
 * 2. Arg1
 * 3. Arity
 *
 * Example:
 * {is_function2,{f,6},{x,0},{x,1}}
 */
public class IsFunction2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg1 = i.arg(1).toArg(proc);
        Term arity = i.arg(2).toArg(proc);
        if (arg1 instanceof Fun) {
            if (arg1.toFun().signature().fun_arity().equals(arity)) {
                return null;
            }
        }
        return i.arg(0);
    }

}
