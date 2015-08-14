package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 *
 */
public class Fmove {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term src = i.arg(0);
        Term dst = i.arg(1);

        if ((src instanceof Float) && dst.isFRegister()) {
            proc.setFR(dst.toRegisterIndex(), src.toFloat());
            return null;
        } else if (src.isFRegister() && dst.isXRegister()) {
            Float srcValue = proc.getFR(src.toRegisterIndex().toInt());
            proc.setX(dst.toRegisterIndex(), srcValue);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
