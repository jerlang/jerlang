package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Convert a term stored in Source into a float and stores in Destination.
 *
 * Arguments:
 * 1. Source
 * 2. Destination
 */
public class Fconv {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term src = i.arg(0);
        Term dst = i.arg(1);

        if (src.isXRegister() && dst.isFRegister()) {
            Term srcValue = src.toArg(proc);
            proc.setFR(dst.toRegisterIndex(), srcValue.toFloat());
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
