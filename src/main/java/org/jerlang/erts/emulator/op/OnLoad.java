package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * on_load is a meta opcode that can be ignored.
 */
public class OnLoad {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        return null;
    }

}
