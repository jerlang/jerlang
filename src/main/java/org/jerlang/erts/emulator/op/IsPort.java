package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.PortID;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a port.
 *
 * Example:
 * {is_port,{f,27},{x,0}}
 */
public class IsPort {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term term = i.arg(1).toArg(proc);
        if (term instanceof PortID) {
            return null;
        } else {
            return i.arg(0);
        }
    }

}
