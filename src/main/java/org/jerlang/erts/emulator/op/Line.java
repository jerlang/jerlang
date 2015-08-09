package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Line instructions are ignored.
 */
public class Line {

    public static Term execute(Process process, Module m, Instruction i, List params) {
        return null;
    }

}
