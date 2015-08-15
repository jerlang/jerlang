package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Deallocate and do a tail call to function of arity Arity
 * pointed to by Destination.
 * Do not update the CP register.
 * Deallocate Deallocate words from the stack before the call.
 */
public class CallExtLast {

    public static Term execute(Process process, Module m, Instruction i, List params) throws ThrowException {
        return CallExt.execute(process, m, i, params);
    }

}
