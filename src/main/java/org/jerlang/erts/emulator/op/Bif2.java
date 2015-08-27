package org.jerlang.erts.emulator.op;

import java.util.ArrayList;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call the bif Bif with the arguments Arg1 and Arg2,
 * and store the result in Reg.
 * On failure jump to Lbl.
 *
 * Arguments:
 * 1. Lbl
 * 2. Bif
 * 3. Arg1
 * 4. Arg2
 * 5. Reg
 *
 * Example:
 * {bif2,{f,0},0,{x,0},{x,1},{x,0}}
 */
public class Bif2 {

    public static Term execute(Process proc, Module m, Instruction i, List params) throws ThrowException {
        Term bif = i.arg(1);
        Term arg1 = i.arg(2).toArg(proc);
        Term arg2 = i.arg(3).toArg(proc);
        Term reg = i.arg(4);

        ArrayList<FunctionSignature> imports = m.beamData().importTableChunk().imports();
        FunctionSignature s = imports.get(bif.toInteger().toInt());

        List args = List.of(arg1, arg2);

        Term result = Erlang.apply(s.module(), s.function(), args);

        // Store result in register
        Integer registerIndex = reg.toRegisterIndex();
        if (reg.isXRegister()) {
            proc.setX(registerIndex, result);
        } else if (reg.isYRegister()) {
            proc.setY(registerIndex, result);
        } else {
            throw new Error("Unsupported destination: " + reg);
        }

        return null;
    }

}
