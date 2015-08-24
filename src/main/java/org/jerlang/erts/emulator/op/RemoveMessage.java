package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Unlink the current message from the message queue and store a
 * pointer to the message in x(0).
 * Remove any timeout.
 */
public class RemoveMessage {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        proc.removeMessage();
        // TODO: remove timeout
        return null;
    }

}
