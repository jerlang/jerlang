package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Deallocate and do a tail recursive call to the function at Label.
 * Do not update the CP register.
 * Before the call deallocate Deallocate words of stack.
 */
public class CallLast {

    public static Term execute(Process process, Module m, Instruction i, List params) {
        throw new Error("not implemented yet");
    }

}
