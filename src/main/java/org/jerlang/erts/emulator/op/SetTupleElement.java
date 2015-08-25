package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Update the element at position Position of the tuple Tuple
 * with the new element NewElement.
 *
 * Arguments:
 * 1. NewElement
 * 2. Tuple
 * 3. Position
 *
 * Example:
 * {set_tuple_element,0,{x,0},1}
 */
public class SetTupleElement {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term newElement = i.arg(0);
        Tuple tuple = i.arg(1).toArg(proc).toTuple();
        Integer position = i.arg(2).toInteger();
        tuple.set(position.toInt(), newElement);
        return null;
    }

}
