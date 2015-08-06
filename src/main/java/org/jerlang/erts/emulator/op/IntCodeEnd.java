package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * This opcode just indicates the end of a code chunk.
 * Just skip this instruction.
 */
public class IntCodeEnd {

    public static Term execute(Process process, Module m, Instruction i, List params) {
        return null;
    }

}
