package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a pid.
 *
 * Arguments:
 * 1. Label
 * 2. Source
 *
 * Example:
 * {is_pid,{f,21},{x,0}}
 */
public class IsPid {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term term = i.arg(1).toArg(proc);
        if (term instanceof PID) {
            return null;
        } else {
            return i.arg(0);
        }
    }

}
