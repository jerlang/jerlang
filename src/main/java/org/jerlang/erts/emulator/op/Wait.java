package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.Process.State;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Suspend the processes and set the entry point to the beginning of the
 * receive loop at Label.
 */
public class Wait {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        proc.setState(State.WAITING);
        return i.arg(0);
    }

}
