package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a tuple.
 */
public class IsTuple {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term lbl = i.arg(0);
        Term arg = i.arg(1);

        if (arg.isXRegister()) {
            Term a = proc.getX(arg.toRegisterIndex());
            if (a instanceof Tuple) {
                return null;
            } else {
                return lbl;
            }
        } else if (arg.isYRegister()) {
            Term a = proc.getY(arg.toRegisterIndex());
            if (a instanceof Tuple) {
                return null;
            } else {
                return lbl;
            }
        } else {
            throw new Error("Can not test arg: " + i);
        }
    }

}
