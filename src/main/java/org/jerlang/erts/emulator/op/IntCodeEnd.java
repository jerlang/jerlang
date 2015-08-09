package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Marker for end of code.
 * This is the last opcode in a code chunk.
 */
public class IntCodeEnd {

    public static Term execute(Process process, Module m, Instruction i, List params) {
        return null;
    }

}
