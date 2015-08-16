package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Check the arity of the tuple Tuple and jump to the corresponding
 * destination label, if no arity matches, jump to FailLabel.
 *
 * Arguments:
 * 1. Tuple
 * 2. FailLabel
 * 3. Destinations
 *
 * Example:
 * {select_tuple_arity,{x,0},{f,14},[1,{f,6},2,{f,7}]}
 */
public class SelectTupleArity {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Tuple tuple = i.arg(0).toArg(proc).toTuple();
        List dst = i.arg(2).toList();
        while (dst.length() > 0) {
            Integer arity = dst.head().toInteger();
            dst = dst.tail();
            Term lbl = dst.head();
            if (tuple.arity() == arity.toInt()) {
                return lbl;
            }
            dst = dst.tail();
        }
        return i.arg(1);
    }

}
