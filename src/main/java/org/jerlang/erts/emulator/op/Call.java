package org.jerlang.erts.emulator.op;

import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;

public class Call {

    public static void execute(Process process, Instruction instruction, List params) {
        System.out.println("Call");
    }

}
