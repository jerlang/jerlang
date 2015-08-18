package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Loop over the message queue, if it is empty jump to Label.
 *
 * Arguments:
 * 1. Label
 * 2. Source
 */
public class LoopRec {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term label = i.arg(0);
        Term source = i.arg(1);
        if (proc.hasMessage()) {
            if (source.isXRegister()) {
                proc.setX(source.toRegisterIndex(), proc.nextMessage());
                return null;
            } else {
                throw new Error("Not implemented: " + i);
            }
        } else {
            return label;
        }
    }

}
