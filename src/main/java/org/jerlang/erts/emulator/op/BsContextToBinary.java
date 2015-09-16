package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Convert the object in SrcDst to a Binary and store it in SrcDst.
 * Usually, SrcDst is a BinMatchState object and this opcode
 * is invoked before a badmatch instruction.
 *
 * Example:
 * {bs_context_to_binary,{x,0}}
 *
 * Arguments:
 * 1. SrcDst
 */
public class BsContextToBinary {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term src = i.arg(0).toArg(proc);
        Term dst = i.arg(0);
        if (src instanceof BinMatchState && dst.isXRegister()) {
            BinMatchState bms = src.toBinMatchState();
            proc.setX(dst.toRegisterIndex(), bms.toBitString());
            return null;
        } else {
            throw new Error("Not implemented: " + i);
        }
    }

}
