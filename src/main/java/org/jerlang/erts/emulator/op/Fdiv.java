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
public class Fdiv {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term s1 = i.arg(1);
        Term s2 = i.arg(2);
        Term dst = i.arg(3);

        if (s1.isFRegister() && s2.isFRegister() && dst.isFRegister()) {
            Float f1 = proc.getFR(s1.toRegisterIndex().toInt());
            Float f2 = proc.getFR(s2.toRegisterIndex().toInt());
            proc.setFR(dst.toRegisterIndex(), f1.divide(f2));
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
