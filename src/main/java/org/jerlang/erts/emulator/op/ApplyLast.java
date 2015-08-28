package org.jerlang.erts.emulator.op;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Apply a function with arity Arity.
 * Function module is stored in X(Arity).
 * Function name is stored in X(Arity+1).
 * Function arguments are stored in X0 ... X(Arity-1).
 *
 * Arguments:
 * 1. Arity
 * 2. FrameSize
 *
 * Example:
 * {apply_last,1,0}
 */
public class ApplyLast {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer arity = i.arg(0).toInteger();
        Atom mod = proc.getX(arity.toInt() + 0).toAtom();
        Atom fun = proc.getX(arity.toInt() + 1).toAtom();

        Module module = ModuleRegistry.get(mod);
        FunctionSignature fs = new FunctionSignature(mod, fun, arity);

        List args = List.nil;
        for (int index = arity.toInt() - 1; index >= 0; index--) {
            args = new List(proc.getX(index), args);
        }

        Term result = module.apply(fs, args);
        proc.setX(0, result);
        return null;
    }

}
