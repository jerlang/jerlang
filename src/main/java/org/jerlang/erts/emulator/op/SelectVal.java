package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Jump to the destination label corresponding to Arg
 * in the Destinations list, if no arity matches, jump to FailLabel.
 *
 * Arguments:
 *
 * 1. Arg
 * 2. FailLabel
 * 3. Destinations
 *
 * Example:
 * {select_val,{x,1},{f,14},[116,{f,3},111,{f,4}]}
 */
public class SelectVal {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg = i.arg(0).toArg(proc);
        List dst = i.arg(2).toList();
        while (dst.length() > 0) {
            Term val = dst.head();
            dst = dst.tail();
            Term lbl = dst.head();
            if (arg.equals(val)) {
                return lbl;
            }
            dst = dst.tail();
        }
        return i.arg(1);
    }

}
