package org.jerlang.erts.emulator.op;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call the function of arity Arity pointed to by Destination.
 * Save the next instruction as the return address in the CP register.
 *
 * First argument is the arity.
 * Second argument is the index of the function in import chunk.
 */
public class CallExt {

    public static Term execute(Process proc, Module m, Instruction i, List params) throws ThrowException {
        Integer arity = i.arg(0).toInteger();
        List parameters = parameters(proc, arity);
        Integer exportTableIndex = i.arg(1).toInteger();
        FunctionSignature s = resolve(m, exportTableIndex);
        Erlang.apply(s.module(), s.function(), parameters);
        // TODO: store result in register
        return null;
    }

    private static List parameters(Process proc, Integer arity) {
        List params = List.nil;
        int maxIndex = arity.toInt();
        for (int index = 0; index < maxIndex; index++) {
            params = new List(proc.registers()[index], params);
        }
        return params;
    }

    private static FunctionSignature resolve(Module m, Integer index) {
        return m.beamData().importTableChunk().imports().get(index.toInt());
    }

}
