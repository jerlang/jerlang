package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Restore the continuation pointer (CP) from the stack and deallocate
 * N+1 words from the stack (the + 1 is for the CP).
 */
public class Deallocate {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        proc.restoreCP();
        proc.deallocate(i.arg(0).toInteger().toInt());
        return null;
    }

}
