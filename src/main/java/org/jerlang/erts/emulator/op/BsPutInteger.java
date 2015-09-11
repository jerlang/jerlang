package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.BitString;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * This opcode assumes that there is a BitString available
 * in process context.
 *
 * Arguments:
 * 1. Label
 * 2. Size
 * 3. Unit
 * 4. Flags
 * 5. Source
 *
 * Example:
 * {bs_put_integer,{f,0},3,1,0,{x,0}}
 */
public class BsPutInteger {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BitString bs = proc.bitString();
        Integer size = i.arg(1).toArg(proc).toInteger();
        Integer unit = i.arg(2).toArg(proc).toInteger();
        Integer flags = i.arg(3).toArg(proc).toInteger();
        Integer source = i.arg(4).toArg(proc).toInteger();
        bs.put_integer(source, size.multiply(unit), flags);
        return null;
    }

}
