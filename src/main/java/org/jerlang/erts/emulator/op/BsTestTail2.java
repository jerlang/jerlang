package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.BinMatchState;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Bitsyntax
 *
 * Test that the matched tail is equal to Bits.
 * Jump to fail if not.
 *
 * Example:
 * {bs_test_tail2,{f,3},{x,1},0}
 *
 * Arg1: Fail
 * Arg2: BinMatchState
 * Arg3: Bits
 */
public class BsTestTail2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms;
        Term arg2 = i.arg(1);

        int bits = i.arg(2).toInteger().toInt();

        if (arg2.isXRegister()) {
            bms = proc.getX(arg2.toTuple().element(2).toInteger()).toBinMatchState();
            if (bms.tail() != bits) {
                return i.arg(0);
            }
        } else {
            throw new Error("Unsupported arg2: " + i);
        }

        return null;
    }

}
