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
    private final int index;

    public Instruction(int index, Opcode opcode, Term... args) {
        super(args.length + 1);
        this.index = index;
        this.opcode = opcode;
        this.args = args;
        set(1, Atom.of(opcode.toString()));
        for (int i = 0; i < args.length; i++) {
            set(2 + i, args[i]);
        }
    }

    public Term arg(int index) {
        return args[index];
    }

    public int arity() {
        return args.length;
    }

    public int index() {
        return index;
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
