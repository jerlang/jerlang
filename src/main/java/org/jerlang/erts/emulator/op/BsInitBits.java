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
 * Create a BitString of size Size with flags Flags and store in Destination.
 *
 * Arguments:
 * 1. Label
 * 2. Size
 * 3. Heap
 * 4. Live
 * 5. Flags
 * 6. Destination
 *
 * Example:
 * {bs_init_bits,{f,0},3,0,1,0,{x,1}}
 */
public class BsInitBits {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        int size = i.arg(1).toInteger().toInt();
        int num = (size / 8) + (size % 8 == 0 ? 0 : 1);
        int[] bytes = new int[num];
        Arrays.fill(bytes, 0);
        int unused = 8 - (size % 8);
        BitString bs = new BitString(bytes, unused);
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
