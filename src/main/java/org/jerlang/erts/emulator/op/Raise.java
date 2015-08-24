package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Arguments:
 * 1. Scope
 * 2. Value
 */
public class Raise {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg1 = i.arg(0).toArg(proc);
        Term arg2 = i.arg(1).toArg(proc);
        throw new Error("RAISE: " + arg1 + ", " + arg2);
    }

}
