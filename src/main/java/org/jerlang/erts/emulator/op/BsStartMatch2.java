package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.BitString;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Bitsyntax
 *
 * Takes a binary and stores a BinMatchState object on the destination.
 * Instead of a binary, Arg2 can also be already a BinMatchState;
 * in that case, copy the BinMatchState.
 *
 * Example:
 * {bs_start_match2,{f,3},{x,0},1,0,{x,1}}
 *
 * Arg1: Fail
 * Arg2: Binary
 * Arg3: X (Live)
 * Arg4: Y (Slots)
 * Arg5: Destination
 */
public class BsStartMatch2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term bmsOrBitString = i.arg(1).toArg(proc);
        Term destination = i.arg(4);
        if (bmsOrBitString instanceof BinMatchState) {
            if (destination.isXRegister()) {
                BinMatchState bms = new BinMatchState(bmsOrBitString.toBinMatchState());
                proc.setX(destination.toRegisterIndex(), bms);
                return null;
            }
        } else if (bmsOrBitString instanceof BitString) {
            int slots = i.arg(3).toInteger().toInt();
            if (destination.isXRegister()) {
                BinMatchState bms = new BinMatchState(bmsOrBitString.toBitString(), slots);
                proc.setX(destination.toRegisterIndex(), bms);
                return null;
            }
        }
        throw new Error("Unsupported instruction: " + i);
    }

}
