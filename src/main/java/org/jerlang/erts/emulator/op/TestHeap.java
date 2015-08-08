package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Ensure there is space for HeapNeed words on the heap. If a GC is needed
 * save Live number of X registers.
 */
public class TestHeap {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        // Not implemented yet
        return null;
    }

}
