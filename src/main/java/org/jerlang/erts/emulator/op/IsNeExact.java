package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

/**
 * Compare two terms and jump to Lbl if Arg1 is exactly equal to Arg2.
 */
public class IsNeExact {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term lbl = i.arg(0);
        Term arg1 = i.arg(1);
        Term arg2 = i.arg(2);

        if (arg1.isXRegister() &&
            (arg2 instanceof Integer | arg2 instanceof Atom | arg2 instanceof Str)) {
            Term a = arg1.toArg(proc);
            Term b = arg2;

            if (a.equals(b)) {
                return lbl;
            } else {
                return null;
            }
        } else {
            throw new Error("Can not compare: " + arg1 + " / " + arg2);
        }
    }

}
