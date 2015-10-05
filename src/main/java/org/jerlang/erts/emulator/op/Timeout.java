package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Reset the save point of the mailbox and clear the timeout flag.
 */
public class Timeout {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        proc.resetMailbox();
        proc.clearTimeout();
        return null;
    }

}
