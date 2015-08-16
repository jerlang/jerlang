package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Reference;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a reference.
 *
 * Example:
 * {is_reference,{f,3},{x,0}}
 */
public class IsReference {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term lbl = i.arg(0);
        Term arg = i.arg(1).toArg(proc);

        if (arg instanceof Reference) {
            return null;
        } else {
            return lbl;
        }
    }

}
