package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.BitString;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Write the value in Source as UTF8 to current BitString stored in process.
 *
 * Arguments:
 * 1. Fail
 * 2. Flags
 * 3. Source
 *
 * Example:
 * {bs_put_utf8,{f,3},0,{x,0}}
 */
public class BsPutUtf8 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BitString bs = proc.bitString();
        Integer value = i.arg(2).toArg(proc).toInteger();
        bs.put_utf8(value);
        return null;
    }

}
