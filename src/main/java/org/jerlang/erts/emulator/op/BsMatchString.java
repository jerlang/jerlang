package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.stdlib.beam_lib.StringTableChunk;
import org.jerlang.type.BitString;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Take a BinMatchState and compare Bits against String chunk data
 * starting at position Val.
 *
 * Arguments:
 * 1. Fail
 * 2. BinMatchState
 * 3. Bits
 * 4. Val
 *
 * Example:
 * {bs_match_string,{f,3},{x,0},8,0}
 */
public class BsMatchString {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms = i.arg(1).toArg(proc).toBinMatchState();
        int length = i.arg(2).toInteger().toInt();
        int offset = i.arg(3).toInteger().toInt();
        StringTableChunk stc = m.beamData().stringTableChunk();
        BitString pattern = stc.strings().extract_bitstring(offset, length);
        if (bms.match(pattern)) {
            return null;
        } else {
            return i.arg(0);
        }
    }

}
