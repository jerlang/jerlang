package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.Opcode;
import org.jerlang.OpcodeTag;
import org.jerlang.erts.emulator.Instruction;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class InstructionReader extends AbstractReader {

    private static final Atom atom = Atom.of("atom");

    private final AtomChunk atomChunk;

    public InstructionReader(DataInputStream inputStream, AtomChunk atomChunk) {
        super(inputStream);
        this.atomChunk = atomChunk;
    }

    public Instruction read(Opcode opcode)
        throws IOException {
        Term[] args = new Term[opcode.arity()];
        for (int index = 0; index < opcode.arity(); index++) {
            args[index] = decodeArg();
        }
        return new Instruction(opcode, args);
    }

    private Term decodeArg() throws IOException {
        int b = read1Byte();
        OpcodeTag tag = OpcodeTag.decode(b);
        switch (tag) {
        case a:
            int atomIndex = decodeInt(b);
            if (atomIndex == 0) {
                return List.nil;
            } else {
                return Tuple.of(atom, atomChunk.atoms()[atomIndex - 1]);
            }
        case u:
            return Integer.of(decodeInt(b));
        case z:
            if ((b & 0x08) != 0) {
                throw new Error("invalid extended tag: " + b);
            }
            switch (b >> 4) {
            case 0:
                throw new Error("decode float not implemented yet");
            case 1:
                throw new Error("decode list not implemented yet");
            case 2:
                throw new Error("decode fr not implemented yet");
            case 3:
                throw new Error("decode allocation list not implemented yet");
            case 4:
                // TODO: literal/float lookup
                Term litIndex = decodeArg();
                System.out.println("Literal at index " + litIndex);
                break;
            }
        default:
            return Tuple.of(tag.toAtom(), Integer.of(decodeInt(b)));
        }
    }

    private int decodeInt(int b) throws IOException {
        // N < 16 = 4 bits, NNNN:0:TTT
        if ((b & 0x08) == 0) {
            return b >> 4;
        }

        // N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
        if ((b & 0x10) == 0) {
            return ((b & 0b11100000) << 3) | read1Byte();
        }

        throw new Error("Unsupported integer"); // TODO
    }

}
