package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.Opcode;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Term;

public class InstructionReader extends AbstractReader {

    private final AtomChunk atomChunk;
    private final LiteralTableChunk literalTableChunk;

    public InstructionReader(DataInputStream inputStream, AtomChunk atomChunk, LiteralTableChunk literalTableChunk) {
        super(inputStream);
        this.atomChunk = atomChunk;
        this.literalTableChunk = literalTableChunk;
    }

    public Instruction read(Opcode opcode)
        throws IOException {
        Term[] args = new Term[opcode.arity()];
        for (int index = 0; index < opcode.arity(); index++) {
            args[index] = decodeArg(atomChunk, literalTableChunk);
        }
        return new Instruction(opcode, args);
    }

}
