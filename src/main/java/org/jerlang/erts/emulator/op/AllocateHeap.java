package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Allocate space for StackNeed words on the stack and ensure there is
 * space for HeapNeed words on the heap. If a GC is needed
 * save Live number of X registers.
 * Also save the continuation pointer (CP) on the stack.
 */
public class AllocateHeap {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        throw new Error("Not implemented: " + i);
    }

}
