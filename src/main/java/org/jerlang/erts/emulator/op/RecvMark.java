package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Save the end of the message queue and the address of
 * the label Label so that a recv_set instruction can start
 * scanning the inbox from this position.
 *
 * Arguments:
 * 1. Label
 *
 * Example:
 * {recv_mark,{f,10}}
 */
public class RecvMark {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        // TODO: Actually save the message queue position and label
        return null;
    }

}
