package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Take a binary from Source and put Size*Unit bytes into current BitString.
 *
 * Arguments:
 * 1. Fail
 * 2. Size
 * 3. Unit
 * 4. Flags
 * 5. Source
 *
 * Example:
 * {bs_put_binary,{f,0},all,8,0,{x,0}}
 */
public class BsPutBinary {

    private static final Atom all = Atom.of("all");

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term size = i.arg(1);
        if (all.equals(size)) {
            Binary binary = i.arg(4).toArg(proc).toBinary();
            proc.bitString().put_binary(binary);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
