package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Normally, this instruction follows a `put_tuple/2` instruction
 * and initializes the previously generated tuple.
 *
 * For example:
 *
 * 1. move(9, {x,1})
 * 2. put_tuple(2, {x,0})
 * 3. put(7)
 * 4. put({x,1})
 *
 * After these instructions are executed,
 * there should be a tuple `{7,9}` in register x0,
 *
 * We need a reference to the tuple in the process,
 * as well as a current index.
 */
public class Put {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Tuple tuple = proc.getTuple();
        if (tuple != null) {
            tuple.set(proc.getTupleIndex() + 1, i.arg(0).toArg(proc));
            proc.incrementTupleIndex();
            if (proc.getTupleIndex() == tuple.arity()) {
                // Remove reference to tuple to allow GC.
                proc.setTuple(null);
            }
            return null;
        } else {
            throw new Error("No current tuple");
        }
    }

}
