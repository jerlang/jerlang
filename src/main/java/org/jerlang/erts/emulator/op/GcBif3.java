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
 * Call the bif Bif with the arguments Arg1, Arg2 and Arg3,
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
 * 5. Arg3
 * 6. Reg
 *
 * Example:
 * {gc_bif3,{f,0},3,0,{x,0},{x,1},{x,2},{x,0}}
 */
public class GcBif3 {

    public static Term execute(Process proc, Module m, Instruction i, List params) {
        Term lbl = i.arg(0); // {f,0}
        Term live = i.arg(1); // 1
        Term bif = i.arg(2); // 1
        Term arg1 = i.arg(3).toArg(proc); // {y,0}
        Term arg2 = i.arg(4).toArg(proc); // {x,0}
        Term arg3 = i.arg(5).toArg(proc); // {x,0}
        Term reg = i.arg(6); // {x,0}

        // TODO: garbage collection

        ArrayList<FunctionSignature> imports = m.beamData().importTableChunk().imports();
        FunctionSignature s = imports.get(bif.toInteger().toInt());

        List args = List.of(arg1, arg2, arg3);
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
