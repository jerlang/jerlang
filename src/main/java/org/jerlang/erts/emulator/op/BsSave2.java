package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Load a BinMatchState from Register.
 * If Index is atom "start", save current offset in save_offset[0].
 * If Index is integer, save current offset in save_offset[Index+1].
 *
 * Arguments:
 * 1. Register
 * 2. Index
 *
 * Example:
 * {bs_save2,{x,0},0}
 */
public class BsSave2 {

    private static final Atom start = Atom.of("start");

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms = i.arg(0).toArg(proc).toBinMatchState();
        Term index = i.arg(1);
        if (index.equals(start)) {
            bms.save(0);
        } else {
            bms.save(index.toInteger().toInt() + 1);
        }
        return null;
    }

}
