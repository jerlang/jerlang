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
 * Create an empty tuple of arity Arity and store in Destination.
 *
 * Arguments:
 * 1. Arity
 * 2. Destination
 */
public class PutTuple {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer arity = i.arg(0).toInteger();
        Term destination = i.arg(1);
        if (destination.isXRegister()) {
            Tuple tuple = new Tuple(arity.toInt());
            proc.setTuple(tuple);
            proc.setX(destination.toRegisterIndex(), tuple);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
