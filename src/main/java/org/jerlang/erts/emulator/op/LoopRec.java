package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Loop over the message queue, if it is empty jump to Label.
 */
public class LoopRec {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("Not implemented: " + i);
    }

}
