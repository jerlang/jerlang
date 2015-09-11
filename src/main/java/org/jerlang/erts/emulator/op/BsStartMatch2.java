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
 *
 * Example:
 * {bs_start_match2,{f,3},{x,0},1,0,{x,1}}
 *
 * Arg1: Fail
 * Arg2: Binary
 * Arg3: X (Live)
 * Arg4: Y (Slots)
 * Arg5: D
 */
public class BsStartMatch2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BitString bitString;
        Term arg2 = i.arg(1);

        if (arg2.isXRegister()) {
            bitString = proc.getX(arg2.toTuple().element(2).toInteger()).toBitString();
        } else {
            throw new Error("Unsupported arg2: " + i);
        }

        int slots = i.arg(3).toInteger().toInt();
        Term destination = i.arg(4);
        if (destination.isXRegister()) {
            BinMatchState bms = new BinMatchState(bitString, slots);
            proc.setX(destination.toTuple().element(2).toInteger(), bms);
            return null;
        } else {
            throw new Error("Unsupported destination: " + i);
        }
    }

}
