package org.jerlang;

import java.util.Map;

import org.jerlang.erts.emulator.Instruction;
import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.beam_lib.BeamData;
import org.jerlang.stdlib.beam_lib.CodeChunk;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

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
        Atom line = Atom.of("line");
        Process process = ProcessRegistry.self();
        CodeChunk code = m.beamData().codeChunk();
        java.util.List<Instruction> instructions = code.instructions();
        Map<Integer, Integer> labels = code.labels();
        int maxInstructions = instructions.size();

        int start = find_start(s, m.beamData());

        params_to_register(params, process.registers());

        Term result = List.nil;

        for (int index = start; index < maxInstructions; index++) {
            Instruction i = instructions.get(index);

            // process.printStack();
            // System.out.println(i);

            if (line.equals(i.element(1))) {
                continue;
            }

            try {
                if (i.opcode().methodHandle() != null) {
                    Term r = (Term) i.opcode().methodHandle().invoke(process, m, i, params);
                    if (r != null) {
                        if (r instanceof org.jerlang.type.Integer) {
                            // CP
                            index = r.toInteger().toInt();
                            if (index == 0) {
                                // Return
                            }
                        } else {
                            // this must be a label jump
                            int lbl = r.toTuple().element(2).toInteger().toInt();
                            index = labels.get(lbl);
                        }
                        // continue;
                    }
                } else {
                    System.err.println("Unsupported opcode: " + i.opcode());
                    break;
                }
            } catch (Throwable e) {
                if (process.exceptionHandler() != null) {
                    Term label = process.exceptionHandler().label();
                    int lbl = label.toRegisterIndex().toInt();
                    index = labels.get(lbl);
                    process.setExceptionHandler(null);
                } else {
                    e.printStackTrace();
                    break;
                }
            }

            if (i.opcode() == Opcode.call_last
                || i.opcode() == Opcode.call_ext_last
                || i.opcode() == Opcode.call_ext_only
                || (i.opcode() == Opcode._return && index == 0)) {
                // The result of the call or return
                // is stored in the x0 register
                result = process.registers()[0];
                break;
            }
        }
        return result;
    }

    /**
     * Store the params, if any, in the X registers of the process.
     */
    private static void params_to_register(List params, Term[] registers) {
        int index = 0;
        while (params.length() > 0) {
            registers[index++] = params.head();
            params = params.tail();
        }
    }

    /**
     * Looks for the exported functions.
     * On match, returns the index of the label instruction
     * that is assigned to the function.
     */
    private static int find_start(FunctionSignature s, BeamData beamData) {
        for (FunctionSignature fs : beamData.exportTableChunk().exports()) {
            if (fs.equals(s)) {
                int lbl = fs.label().toInt();
                int idx = beamData.codeChunk().labels().get(lbl);
                return idx;
            }
        }
        throw new Error("No label found for " + s);
    }
}
