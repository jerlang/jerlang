package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Clear the Nth stack word. (By writing NIL.)
 *
 * Example:  {init,{y,0}}
 * Alias:    {kill,{y,0}}
 */
public class Init {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg = i.arg(0);

        if (arg.isYRegister()) {
            proc.setY(arg.toRegisterIndex(), List.nil);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
