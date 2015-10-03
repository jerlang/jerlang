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
 * Call the bif Bif with the arguments Arg1 and Arg2,
 * and store the result in Reg.
 * On failure jump to Lbl.
 * Do a garbage collection if necessary to allocate space on the heap
 * for the result (saving Live number of X registers).
 *
 * Arguments:
 * 1. Lbl
 * 2. Live
 * 3. Bif
 * 4. Arg1
 * 5. Arg2
 * 6. Reg
 */
public class GcBif2 {

    public static Term execute(Process process, Module m, Instruction i, List params) throws ThrowException {
        Term bif = i.arg(2); // 1
        Term arg1 = i.arg(3); // {y,0}
        Term arg2 = i.arg(4); // {x,0}
        Term reg = i.arg(5); // {x,0}

        // TODO: garbage collection

        ArrayList<FunctionSignature> imports = m.beamData().importTableChunk().imports();
        FunctionSignature s = imports.get(bif.toInteger().toInt());

        List args = List.nil;

        if (arg2 instanceof Integer) {
            args = new List(arg2, args);
        } else if (arg2.isXRegister()) {
            args = new List(arg2.toArg(process), args);
        } else {
            throw new Error("Unsupported ARG2: " + arg2);
        }

        if (arg1.isXRegister() || arg1.isYRegister()) {
            args = new List(arg1.toArg(process), args);
        } else if (arg1 instanceof Integer) {
            args = new List(arg1, args);
        } else {
            throw new Error("Unsupported ARG1: " + arg1);
        }

        Term result = Erlang.apply(s.module(), s.function(), args);

        // Store result in register
        Integer registerIndex = reg.toRegisterIndex();
        if (reg.isXRegister()) {
            process.setX(registerIndex, result);
        } else if (reg.isYRegister()) {
            process.setY(registerIndex, result);
        } else {
            throw new Error("Unsupported destination: " + reg);
        }
        return null;
    }

}
