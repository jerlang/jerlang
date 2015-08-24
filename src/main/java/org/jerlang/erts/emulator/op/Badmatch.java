package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Raise badmatch error.
 *
 * Arg1: term which caused the problem
 */
public class Badmatch {

    private static final Atom badmatch = Atom.of("badmatch");

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term arg1 = i.arg(0);
        if (arg1.isXRegister()) {
            Term cause = arg1.toArg(proc);
            throw new Error(Tuple.of(badmatch, cause));
        } else {
            throw new Error("Unsupported arg1: " + i);
        }
    }

}
