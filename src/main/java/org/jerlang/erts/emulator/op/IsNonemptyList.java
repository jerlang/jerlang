package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a cons.
 *
 * Arguments:
 * 1. Lbl
 * 2. Arg1
 */
public class IsNonemptyList {

    private static final Atom x = Atom.of("x");

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term lbl = i.arg(0);
        Term arg1 = i.arg(1);

        if (arg1 instanceof Tuple && x.equals(arg1.toTuple().element(1))) {
            arg1 = proc.getX(arg1.toTuple().element(2).toInteger());
        }

        if (arg1 instanceof List && arg1.toList().length() > 0) {
            return null;
        } else {
            return lbl;
        }
    }
}
