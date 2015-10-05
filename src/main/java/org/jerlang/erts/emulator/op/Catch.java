package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.CatchExceptionHandler;
import org.jerlang.type.stack.ExceptionHandler;

/**
 *
 */
public class Catch {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term destination = i.arg(0);
        if (destination.isYRegister()) {
            Term label = i.arg(1);
            ExceptionHandler eh = new CatchExceptionHandler(label);
            proc.setY(destination.toRegisterIndex(), eh);
            proc.setExceptionHandler(eh);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
