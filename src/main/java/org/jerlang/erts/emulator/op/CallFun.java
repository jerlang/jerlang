package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call a fun of arity Arity. Assume arguments in
 * registers x(0) to x(Arity-1) and that the fun is in x(Arity).
 * Save the next instruction as the return address in the CP register.
 */
public class CallFun {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("Not implemented: " + i);
    }

}
