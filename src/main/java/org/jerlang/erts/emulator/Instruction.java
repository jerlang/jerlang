package org.jerlang.erts.emulator;

import org.jerlang.Opcode;
import org.jerlang.type.Term;

/**
 * https://github.com/erlang/otp/blob/master/erts/emulator/beam/ops.tab
 * https://github.com/erlang/otp/blob/master/erts/emulator/utils/beam_makeops
 */
public class Instruction {

    private final Opcode opcode;
    private final Term[] args;

    public Instruction(Opcode opcode, Term... args) {
        this.opcode = opcode;
        this.args = args;
    }

    public Term arg(int index) {
        return args[index];
    }

    public int arity() {
        return args.length;
    }

    public Opcode opcode() {
        return opcode;
    }

}
