package org.jerlang.erts.emulator.op;

import java.util.Arrays;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.BitString;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Create a BitString of Size bytes with flags Flags and store in Destination.
 *
 * Arguments:
 * 1. Fail
 * 2. Size
 * 3. Words (ignored)
 * 4. Registers (ignored)
 * 5. Flags
 * 6. Destination
 *
 * Example:
 * {bs_init2,{f,3},{x,2},0,2,0,{x,2}}
 */
public class BsInit2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        int size = i.arg(1).toArg(proc).toInteger().toInt();
        int[] bytes = new int[size];
        Arrays.fill(bytes, 0);
        BitString bs = new BitString(bytes);
        proc.setBitString(bs);
        Term destination = i.arg(5);
        if (destination.isXRegister()) {
            proc.setX(destination.toRegisterIndex(), bs);
        } else {
            throw new Error("Not implemented: " + i);
        }
        return null;
    }

}
