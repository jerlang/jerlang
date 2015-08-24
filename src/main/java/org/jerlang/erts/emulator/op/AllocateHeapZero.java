package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Allocate space for StackNeed words on the stack and HeapNeed words
 * on the heap. If a GC is needed
 * during allocation there are Live number of live X registers.
 * Clear the new stack words. (By writing NIL.)
 * Also save the continuation pointer (CP) on the stack.
 *
 * Arguments:
 * 1. StackNeed
 * 2. HeapNeed
 * 3. Live
 */

public class AllocateHeapZero {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Integer stack = i.arg(0).toInteger();
        Integer heap = i.arg(1).toInteger();
        Integer live = i.arg(2).toInteger();

        if (stack.toInt() > 0) {
            proc.allocate_heap_zero(stack.toInt(), heap.toInt(), live.toInt());
        }

        proc.storeCP();
        return null;
    }

}
