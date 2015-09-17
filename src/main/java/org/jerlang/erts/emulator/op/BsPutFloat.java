package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Put a float Value as Size*Unit bits into current bitstring.
 * Unit seems to be always 1.
 *
 * Arguments:
 * 1. Fail
 * 2. Size
 * 3. Unit
 * 4. Flags
 * 5. Value
 *
 * Example:
 * {bs_put_float,{f,0},64,1,0,{x,0}}
 */
public class BsPutFloat {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        int size = i.arg(1).toInteger().toInt();
        int flag = i.arg(3).toInteger().toInt();
        Float value = i.arg(4).toArg(proc).toFloat();
        proc.bitString().put_float(value, size, flag);
        return null;
    }

}
