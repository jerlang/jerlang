package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Load a BinMatchState from Register and set its offset to Index.
 * Index can be the atom `start`, meaning index at position 0.
 * Otherwise, Index is an integer, meaning index at position Index+1.
 *
 * Arguments:
 * 1. Register
 * 2. Index
 *
 * Example:
 * {bs_restore2,{x,0},start}
 */
public class BsRestore2 {

    private static final Atom start = Atom.of("start");

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms = i.arg(0).toArg(proc).toBinMatchState();
        Term index = i.arg(1);
        if (index.equals(start)) {
            bms.restore(0);
        } else {
            bms.restore(index.toInteger().toInt() + 1);
        }
        return null;
    }

}
