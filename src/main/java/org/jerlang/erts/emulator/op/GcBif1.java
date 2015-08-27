package org.jerlang.erts.emulator.op;

import java.util.ArrayList;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.Process;
import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * Call the bif Bif with the argument Arg, and store the result in Reg.
 * On failure jump to Lbl.
 * Do a garbage collection if necessary to allocate space on the heap
 * for the result (saving Live number of X registers).
 *
 * Arguments:
 * 1. Lbl
 * 2. Live
 * 3. Bif
 * 4. Arg
 * 5. Reg
 *
 * Example:
 * {gc_bif1,{f,0},1,0,{x,0},{x,1}}
 */
public class GcBif1 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term bif = i.arg(2);
        Term arg = i.arg(3);
        Term reg = i.arg(4);

        // TODO: garbage collection

        ArrayList<FunctionSignature> imports = m.beamData().importTableChunk().imports();
        FunctionSignature s = imports.get(bif.toInteger().toInt());

        List args = List.of(arg.toArg(proc));
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
