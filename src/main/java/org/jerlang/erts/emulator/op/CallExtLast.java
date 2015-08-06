package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;

public class CallExtLast {

    public static void execute(Process process, Module m, Instruction i, List params) {
        CallExt.execute(process, m, i, params);
    }

}
