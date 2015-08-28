package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Deallocate and do a tail recursive call to the function at Label.
 * Do not update the CP register.
 * Before the call deallocate Deallocate words of stack.
 *
 * Arguments:
 * 1. Arity
 * 2. Label
 * 3. Deallocate
 *
 * Example:
 * {call_last,2,{f,4},1}
 */
public class CallLast {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer arity = i.arg(0).toInteger();
        Term label = i.arg(1);
        Integer deallocate = i.arg(2).toInteger();

        proc.deallocate(deallocate.toInt());
        return label;
    }

}
