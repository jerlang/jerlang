package org.jerlang.erts.emulator.op;

import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Float;
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
 */
public class BsGetFloat2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        BinMatchState bms;
        Term arg2 = i.arg(1);

        if (arg2.isXRegister()) {
            bms = arg2.toArg(proc).toBinMatchState();
        } else {
            throw new Error("Unsupported arg2: " + i);
        }

        int size = i.arg(3).toInteger().toInt();
        int unit = i.arg(4).toInteger().toInt();
        int flag = i.arg(5).toInteger().toInt();

        Float result = bms.get_float(size, unit, flag);

        Term destination = i.arg(6);
        if (destination.isXRegister()) {
            proc.setX(destination.toRegisterIndex(), result);
            return null;
        } else {
            throw new Error("Unsupported destination: " + i);
        }
    }

}
