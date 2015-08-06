package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class Move {

    private final static Atom x = Atom.of("x");

    public static void execute(Process proc, Module m, Instruction i, List params) {
        Term source = i.arg(0);
        Term destination = i.arg(1);

        if (destination instanceof Tuple) {
            Tuple destinationTuple = destination.toTuple();
            if (destinationTuple.arity() != 2) {
                throw new Error("Unsupported destination: " + destination);
            }
            if (!x.equals(destinationTuple.element(1))) {
                throw new Error("Unsupported destination: " + destination);
            }
            Integer registerIndex = destinationTuple.element(2).toInteger();
            proc.registers()[registerIndex.toInt()] = source;
        } else {
            throw new Error("Unsupported destination: " + destination);
        }
    }

}
