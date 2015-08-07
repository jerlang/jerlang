package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Do a tail recursive call to the function at Label.
 * Do not update the CP register.
 *
 * Arguments:
 * 1. Arity
 * 2. Label
 */
public class CallOnly {

    public static Term execute(Process process, Module m, Instruction i, List params) {
        return i.arg(1);
    }

}
