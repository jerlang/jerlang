package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Takes a BinMatchState in Source and tests if its remaining bits
 * are divisible by unit without rest.
 *
 * Arguments:
 * 1. Fail Label
 * 2. Source
 * 3. Unit
 *
 * Example:
 * {bs_test_unit,{f,6},{x,0},8}
 */
public class BsTestUnit {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms = i.arg(1).toArg(proc).toBinMatchState();
        int unit = i.arg(2).toInteger().toInt();
        return bms.test_unit(unit) ? null : i.arg(0);
    }

}
