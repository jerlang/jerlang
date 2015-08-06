package org.jerlang.erts.emulator;

import org.jerlang.Opcode;
import org.jerlang.type.Atom;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * https://github.com/erlang/otp/blob/master/erts/emulator/beam/ops.tab
 * https://github.com/erlang/otp/blob/master/erts/emulator/utils/beam_makeops
 */
public class Instruction extends Tuple {

    private final Opcode opcode;
    private final Term[] args;

    public Instruction(Opcode opcode, Term... args) {
        super(args.length + 1);
        this.opcode = opcode;
        this.args = args;
        set(1, Atom.of(opcode.toString()));
        for (int index = 0; index < args.length; index++) {
            set(2 + index, args[index]);
        }
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

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("{");
        stringBuilder.append(opcode);
        stringBuilder.append(",");
        for (Term arg : args) {
            stringBuilder.append(arg).append(",");
        }
        stringBuilder.setLength(stringBuilder.length() - 1);
        stringBuilder.append("}");
        return stringBuilder.toString();
    }

}
