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
 * 3. Extra
 * 4. Live
 * 5. Unit
 * 6. Bin
 * 7. Flags
 * 8. Dst
 *
 * Example:
 * {bs_append,{f,0},8,0,0,8,{x,0},0,{x,1}}
 */
public class BsAppend {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        // TODO: This covers only the basic case, create more unit tests to expand functionality
        int bits = i.arg(1).toInteger().toInt();
        BitString bs = i.arg(5).toArg(proc).toBitString().append(bits);
        Term dst = i.arg(7);
        if (dst.isXRegister()) {
            proc.setBitString(bs);
            proc.setX(dst.toRegisterIndex(), bs);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
