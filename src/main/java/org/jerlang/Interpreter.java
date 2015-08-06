package org.jerlang;

import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * Executes opcodes
 */
public class Interpreter {

    public static Term dispatch(List params) {
        // A call to a function defined in a BEAM file
        // has always the signature as first parameter.
        FunctionSignature s = params.head().toFunctionSignature();
        params = params.tail();
        // Lookup the module to get the code
        Module m = ModuleRegistry.get(s.module());
        boolean skip = true;
        Tuple label = Tuple.of(Atom.of("label"), s.label());
        Atom line = Atom.of("line");
        Process process = ProcessRegistry.self();
        for (Instruction i : m.beamData().codeChunk().instructions()) {
            if (label.equals(i)) {
                skip = false;
                continue;
            }
            if (line.equals(i.element(1))) {
                continue;
            }
            if (skip) {
                continue;
            }

            try {
                if (i.opcode().methodHandle() != null) {
                    i.opcode().methodHandle().invoke(process, m, i, params);
                } else {
                    System.err.println("Unsupported opcode: " + i.opcode());
                }
            } catch (Throwable e) {
                e.printStackTrace();
            }

            if (i.element(1).toString().endsWith("_last")) {
                break;
            }
        }
        return null;
    }
}
