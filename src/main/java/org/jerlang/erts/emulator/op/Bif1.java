package org.jerlang.erts.emulator.op;

import java.util.ArrayList;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call the bif Bif with the argument Arg, and store the result in Reg.
 * On failure jump to Lbl.
 *
 * Arguments:
 * 1. Lbl
 * 2. Bif
 * 3. Arg
 * 4. Reg
 *
 * Example:
 * {bif1,{f,0},0,{x,0},{x,0}}
 */
public class Bif1 {

    public static Term execute(Process proc, Module m, Instruction i, List params) throws ThrowException {
        Term bif = i.arg(1);
        Term arg = i.arg(2).toArg(proc);
        Term reg = i.arg(3);

        ArrayList<FunctionSignature> imports = m.beamData().importTableChunk().imports();
        FunctionSignature s = imports.get(bif.toInteger().toInt());

        List args = List.of(arg);

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
