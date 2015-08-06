package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Allocate space for StackNeed words on the stack. If a GC is needed
 * during allocation there are Live number of live X registers.
 * Also save the continuation pointer (CP) on the stack.
 */
public class Allocate {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer stackNeed = i.arg(0).toInteger();
        Integer live = i.arg(1).toInteger();

        if (stackNeed.toInt() > 0) {
            throw new Error("Stack allocation is not implemented yet");
        }

        if (live.toInt() > 0) {
            throw new Error("Live support is not implemented yet");
        }

        // TODO: Save continuation pointer
        return null;
    }

}
