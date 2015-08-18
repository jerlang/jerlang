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
 * Call the bif Bif and store the result in Reg.
 *
 * Arguments:
 * 1. Bif
 * 2. Reg
 *
 * Example:
 * {bif0,0,{x,0}}
 */
public class Bif0 {

    public static Term execute(Process proc, Module m, Instruction i, List params) throws ThrowException {
        Term bif = i.arg(0);
        Term reg = i.arg(1);

        ArrayList<FunctionSignature> imports = m.beamData().importTableChunk().imports();
        FunctionSignature s = imports.get(bif.toInteger().toInt());

        Term result = Erlang.apply(s.module(), s.function(), List.nil);

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
