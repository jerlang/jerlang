package org.jerlang.erts.emulator.op;

import java.util.Map;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.beam_lib.CodeChunk;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.ExceptionHandler;

/**
 * Arguments:
 *
 * 1. Where to put the handler
 * 2. Catch Label
 */
public class Try {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term destination = i.arg(0);
        if (destination.isYRegister()) {
            CodeChunk code = m.beamData().codeChunk();
            Map<Integer, Integer> labels = code.labels();

            Term label = i.arg(1);
            int index = labels.get(label);

            ExceptionHandler eh = new ExceptionHandler(label, index);
            proc.setY(destination.toRegisterIndex(), eh);
            proc.setExceptionHandler(eh);
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
