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

    private static final Atom LINE = Atom.of("line");

    public static Term dispatch(List params) {
        // A call to a function defined in a BEAM file
        // has always the signature as first parameter.

        Process process = ProcessRegistry.self();
        FunctionSignature s = process.signature();

        params_to_register(params, process.registers());

        Module m = ModuleRegistry.get(s.module());
        int start = find_start(s, m.beamData());

        return continueExecution(s, start);
    }

    public static Term continueExecution(FunctionSignature signature, int start) {
        Process process = ProcessRegistry.self();

        Module m = ModuleRegistry.get(signature.module());

        if (m.isInternal()) {
            // We need to fetch params from registers
            List args = List.nil;
            for (int index = signature.fun_arity().toInt(); index > 0; index--) {
                args = new List(process.getX(index - 1), args);
            }
            Term res = m.apply(signature, args);
            process.setX(0, res);
            // Setting the EXITING state must be the last
            // operation before return. Otherwise another
            // thread waiting for this process might get
            // wrong result as X0 is not filled yet.
            process.setState(ProcessState.EXITING);
            return res;
        }

        CodeChunk code = m.beamData().codeChunk();
        java.util.List<Instruction> instructions = code.instructions();
        Map<Integer, Integer> labels = code.labels();
        int maxInstructions = instructions.size();

        if (start == -1) {
            start = find_start(process.signature(), m.beamData());
        }

        Term result = List.nil;
        Term params = List.nil; // TODO: is this correct?

        for (int index = start; index < maxInstructions; index++) {
            Instruction i = instructions.get(index);

            // process.printStack();
            // System.out.println(i);

            if (LINE.equals(i.element(1))) {
                continue;
            }

            try {
                // System.out.println("" + process + ": " + String.format("%2d", index) + ": " + i);

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
                            // this must be a label jump (e.g. "{f,1}")
                            int lbl = r.toRegisterIndex().toInt();
                            index = labels.get(lbl);
                        }
                    }
                } else {
                    System.err.println("Unsupported opcode: " + i.opcode());
                    break;
                }
            } catch (Error error) {
                if (process.exceptionHandler() != null) {
                    // System.err.println(process.exceptionHandler());
                    int new_index = process.exceptionHandler().index();
                    if (new_index >= 0) {
                        index = new_index;
                    }
                    process.exceptionHandler().handle(process, error);
                    process.setExceptionHandler(null);
                } else {
                    System.err.println("Raise catched");
                    error.printStackTrace();
                    System.exit(1);
                    break;
                }
            } catch (Throwable e) {
                System.err.println("Index: " + index);
                e.printStackTrace();
                System.exit(1);
                break;
            }

            if (process.state() == ProcessState.WAITING) {
                // Set process was set to waiting mode.
                process.setCP(index);
                break;
            }

            Opcode opcode = i.opcode();

            if (opcode == Opcode.apply_last
                || opcode == Opcode.call_ext_last
                || opcode == Opcode.call_ext_only
                || (opcode == Opcode._return && index <= 0)) {
                // The result of the call or return
                // is stored in the x0 register
                result = process.registers()[0];
                process.setState(ProcessState.EXITING);
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
