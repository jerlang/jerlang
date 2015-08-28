package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Reduce the stack usage by N words,
 * keeping the CP on the top of the stack.
 *
 * Arguments:
 * 1. N
 * 2. Remaining
 *
 * Example:
 * {trim,1,1}
 */
public class Trim {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer n = i.arg(0).toInteger();
        proc.deallocate(n.toInt());
        return null;
    }

}
