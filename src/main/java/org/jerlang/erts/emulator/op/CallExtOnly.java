package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Do a tail recursive call to the function at Label.
 * Do not update the CP register.
 */
public class CallExtOnly {

    public static Term execute(Process process, Module m, Instruction i, List params) {
        return CallExt.execute(process, m, i, params);
    }

}
