package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Fun;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call a fun of arity Arity. Assume arguments in
 * registers x(0) to x(Arity-1) and that the fun is in x(Arity).
 * Save the next instruction as the return address in the CP register.
 *
 * Arguments:
 * 1. Arity
 *
 * Example:
 * {call_fun,1}
 */
public class CallFun {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer arity = i.arg(0).toInteger();
        Fun fun = proc.getX(arity).toFun();
        // Restore saved registers, if any
        int regIndex = arity.toInt();
        for (int index = 0; index < fun.savedRegisters().length; index++) {
            proc.setX(regIndex + index, fun.savedRegisters()[index]);
        }
        proc.setCP(i.index() + 1);
        return fun.label();
    }

}
