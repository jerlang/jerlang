package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Check that the saved mark points to Label and set the
 * save pointer in the message queue to the last position
 * of the message queue saved by the recv_mark instruction.
 *
 * Arguments:
 * 1. Label
 *
 * Example:
 * {recv_set,{f,10}}
 */
public class RecvSet {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        // TODO: Actually implement the check and restore of msg queue ptr.
        // TODO: It is safe do just skip this statement as it is only a performance optimization.
        return null;
    }

}
