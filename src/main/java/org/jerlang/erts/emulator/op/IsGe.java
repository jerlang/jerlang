package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * is greater or equal
 *
 * Arguments:
 * 1. Lbl
 * 2. Arg1
 * 3. Arg2
 *
 * Compare two terms and jump to Lbl if Arg1 is less than Arg2.
 */
public class IsGe {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term lbl = i.arg(0);
        Term arg1 = i.arg(1);
        Term arg2 = i.arg(2);

        if (arg1 instanceof Tuple && arg2 instanceof Integer) {
            int registerIndex = arg1.toTuple().element(2).toInteger().toInt();
            Integer a = proc.registers()[registerIndex].toInteger();
            Integer b = arg2.toInteger();
            if (a.toInt() < b.toInt()) {
                return lbl;
            } else {
                return null;
            }
        } else {
            throw new Error("Can not compare: " + arg1 + " / " + arg2);
        }
    }

}
