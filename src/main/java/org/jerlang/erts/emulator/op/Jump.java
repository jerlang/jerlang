package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Jump to Label.
 */
public class Jump {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        return i.arg(0);
    }

}
