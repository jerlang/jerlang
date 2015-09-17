package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Store (Source1 + (Source2 * Unit)) in Destination.
 *
 * Arguments:
 * 1. Label
 * 2. Source1
 * 3. Source2
 * 4. Unit
 * 5. Destination
 *
 * Example:
 * {bs_add,{f,3},{x,2},{x,3},1,{x,2}}
 */
public class BsAdd {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer source1 = i.arg(1).toArg(proc).toInteger();
        Integer source2 = i.arg(2).toArg(proc).toInteger();
        Integer unit = i.arg(3).toArg(proc).toInteger();
        Integer result = source1.add(source2.multiply(unit));
        Term destination = i.arg(4);
        if (destination.isXRegister()) {
            proc.setX(destination.toRegisterIndex(), result);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
