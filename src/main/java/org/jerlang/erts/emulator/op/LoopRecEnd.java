package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Advance the save pointer to the next message and jump back to Label.
 *
 * Example:
 * {loop_rec_end,{f,10}}
 */
public class LoopRecEnd {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        proc.loop_rec_end();
        return i.arg(0);
    }

}
