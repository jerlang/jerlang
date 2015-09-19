package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.BitString;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Initializes a writable BitString.
 * BitString size is stored in X0 register.
 * Writable BitString is stored in X0 and process.
 *
 * Arguments:
 * None
 *
 * Example:
 * {bs_init_writable}
 */
public class BsInitWritable {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer size = proc.getX(0).toInteger();
        BitString bs = new BitString(0, true);
        proc.setBitString(bs);
        proc.setX(0, bs);
        return null;
    }

}
