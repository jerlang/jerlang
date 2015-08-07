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
    private final static Atom y = Atom.of("y");

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term source = i.arg(0);
        Term destination = i.arg(1);

        if (source instanceof Tuple) {
            Tuple sourceTuple = source.toTuple();
            if (sourceTuple.arity() != 2) {
                throw new Error("Unsupported source: " + source);
            }
            Integer registerIndex = sourceTuple.element(2).toInteger();
            if (x.equals(sourceTuple.element(1))) {
                source = proc.registers()[registerIndex.toInt()];
            } else if (y.equals(sourceTuple.element(1))) {
                source = proc.getY(registerIndex);
                // source = proc.popStack();
            } else {
                throw new Error("Unsupported destination: " + destination);
            }
        }

        if (destination instanceof Tuple) {
            Tuple destinationTuple = destination.toTuple();
            if (destinationTuple.arity() != 2) {
                throw new Error("Unsupported destination: " + destination);
            }
            Integer registerIndex = destinationTuple.element(2).toInteger();
            if (x.equals(destinationTuple.element(1))) {
                proc.registers()[registerIndex.toInt()] = source;
            } else if (y.equals(destinationTuple.element(1))) {
                proc.setY(registerIndex, source);
                // proc.pushStack(source);
            } else {
                throw new Error("Unsupported destination: " + destination);
            }
            return null;
        } else {
            throw new Error("Unsupported destination: " + destination);
        }
    }

}
