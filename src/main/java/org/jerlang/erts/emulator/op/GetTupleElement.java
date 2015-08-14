package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Get element number Element from the tuple in Source and put
 * it in the destination register Destination.
 *
 * Note that Element index starts at 0.
 *
 * Arguments:
 * 1. Source
 * 2. Element
 * 3. Destination
 */
public class GetTupleElement {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term destination = i.arg(2);
        if (destination.isXRegister()) {
            Tuple tuple = i.arg(0).toArg(proc).toTuple();
            Integer element = i.arg(1).toInteger();
            proc.setX(destination.toRegisterIndex(), tuple.element(element.toInt() + 1));
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
