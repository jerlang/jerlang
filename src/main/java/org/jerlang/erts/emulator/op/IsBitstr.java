package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Test the type of Arg1 and jump to Lbl if it is not a bit string.
 */
public class IsBitstr {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("not implemented yet: " + i);
    }

}
