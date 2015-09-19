package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a port.
 *
 * Example:
 * {is_port,{f,27},{x,0}}
 */
public class IsPort {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        // TODO: Ports are not implemented yet,
        // TODO: so this opcode always returns the fail label
        return i.arg(0);
    }

}
