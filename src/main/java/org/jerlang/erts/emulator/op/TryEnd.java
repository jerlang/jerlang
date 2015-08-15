package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * We have passed a try block without exception,
 * so we can remove the exception handler from the stack.
 */
public class TryEnd {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term source = i.arg(0);
        if (source.isYRegister()) {
            // We could fetch the handler from stack
            // and compare to current handler to make
            // sure that everything is ok, but for
            // now, we just reset the current handler.
            proc.setExceptionHandler(null);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
