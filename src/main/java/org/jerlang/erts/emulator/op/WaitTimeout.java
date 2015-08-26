package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.ProcessState;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Sets up a timeout of Time milliseconds and saves the address of the
 * following instruction as the entry point if the timeout triggers.
 */
public class WaitTimeout {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        proc.setState(ProcessState.WAITING);
        // TODO: Set timeout
        return i.arg(0);
    }

}
