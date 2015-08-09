package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Reduce the stack usage by N words,
 * keeping the CP on the top of the stack.
 */
public class Trim {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("Not implemented: " + i);
    }

}
