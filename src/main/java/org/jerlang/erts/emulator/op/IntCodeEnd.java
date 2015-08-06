package org.jerlang.erts.emulator.op;

import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;

/**
 * This opcode just indicates the end of a code chunk.
 * Just skip this instruction.
 */
public class IntCodeEnd {

    public static void execute(Process process, Instruction instruction, List params) {
    }

}
