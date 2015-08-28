package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Return to the address in the continuation pointer (CP).
 */
public class Return {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer cp = Integer.of(proc.getCP());
        proc.setCP(0); // reset
        return cp;
    }

}
