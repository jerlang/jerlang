package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Raise badmatch error.
 *
 * Arg1: term which caused the problem
 */
public class Badmatch {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg1 = i.arg(0);
        if (arg1.isXRegister()) {
            Term cause = proc.getX(arg1.toTuple().element(2).toInteger());
            throw new Error("badmatch: " + cause);
        } else {
            throw new Error("Unsupported arg1: " + i);
        }
    }

}
