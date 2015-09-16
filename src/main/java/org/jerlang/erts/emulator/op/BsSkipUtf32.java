package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Skip Size UTF32 chars in BinMatchState defined in Source.
 *
 * Arguments:
 * 1. Label
 * 2. Source
 * 3. Size
 * 4. Flags
 *
 * Example:
 * {bs_skip_utf32,{f,9},{x,0},1,0}
 */
public class BsSkipUtf32 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms = i.arg(1).toArg(proc).toBinMatchState();
        int size = i.arg(2).toInteger().toInt();
        int flag = i.arg(3).toInteger().toInt();
        return bms.skip_utf32(size, flag) ? null : i.arg(0);
    }

}
