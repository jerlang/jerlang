package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Specify a module local label.
 * Label gives this code address a name (Lbl)
 * and marks the start of a basic block.
 *
 * Label instructions are ignored.
 * Labels are collected during initial load of the code chunk.
 */
public class Label {

    public static Term execute(Process process, Module m, Instruction i, List params) {
        return null;
    }

}
