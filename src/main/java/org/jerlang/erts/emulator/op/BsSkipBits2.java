package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 *
 * Arguments:
 * 1. Fail Label
 * 2. Source
 * 3. Size
 * 4. Unit
 * 5. Flags
 *
 * Example:
 * {bs_skip_bits2,{f,3},{x,0},8,1,0}
 */
public class BsSkipBits2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms = i.arg(1).toArg(proc).toBinMatchState();
        int size = i.arg(2).toInteger().toInt();
        int unit = i.arg(3).toInteger().toInt();
        int flag = i.arg(4).toInteger().toInt();
        bms.skip(size, unit, flag);
        return null;
    }

}
