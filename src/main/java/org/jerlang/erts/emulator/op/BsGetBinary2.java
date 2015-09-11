package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.BitString;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.stack.BinMatchState;

/**
 * Arguments:
 * 1. Label
 * 2. BinMatchState
 * 3. Live
 * 4. Size
 * 5. Unit
 * 6. Flags
 * 7. Destination
 *
 * Example:
 * {bs_get_binary2,{f,3},{x,1},3,all,1,0,{x,1}}
 */
public class BsGetBinary2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms;
        Term arg2 = i.arg(1);
        Term arg4 = i.arg(3);

        if (arg2.isXRegister()) {
            bms = arg2.toArg(proc).toBinMatchState();
        } else {
            throw new Error("Unsupported arg2: " + i);
        }

        if (arg4.equals(Atom.of("all"))) {
            int unit = i.arg(4).toInteger().toInt();
            int flag = i.arg(5).toInteger().toInt();

            BitString result = bms.get_all_binary(unit, flag);

            Term destination = i.arg(6);
            if (destination.isXRegister()) {
                proc.setX(destination.toTuple().element(2).toInteger(), result);
                return null;
            } else {
                throw new Error("Unsupported destination: " + i);
            }
        } else {
            throw new Error("Unsupported size: " + i);
        }
    }

}
