package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.BitString;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Arguments:
 * 1. Fail
 * 2. Size
 * 3. Unit
 * 4. Bin
 * 5. Flags
 * 6. Dst
 *
 * Example:
 * {bs_private_append,{f,0},8,1,{x,1},0,{x,3}}
 */
public class BsPrivateAppend {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        // TODO: This covers only the basic case, create more unit tests to expand functionality
        int bits = i.arg(1).toInteger().toInt();
        BitString bs = i.arg(3).toArg(proc).toBitString().append(bits);
        Term dst = i.arg(5);
        if (dst.isXRegister()) {
            proc.setBitString(bs);
            proc.setX(dst.toRegisterIndex(), bs);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
