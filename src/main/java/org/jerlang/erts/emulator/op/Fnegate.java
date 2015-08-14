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
public class Fnegate {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term s1 = i.arg(1);
        Term dst = i.arg(2);

        if (s1.isFRegister() && dst.isFRegister()) {
            Float f1 = proc.getFR(s1.toRegisterIndex().toInt());
            proc.setFR(dst.toRegisterIndex(), f1.negate());
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
