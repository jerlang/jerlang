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

public class InstructionReader {

    public static Instruction read(Opcode opcode, DataInputStream inputStream)
        throws IOException {
        Term[] args = new Term[opcode.arity()];
        for (int index = 0; index < opcode.arity(); index++) {
            args[index] = decodeArg(inputStream);
        }
        return new Instruction(opcode, args);
    }

    private static Term decodeArg(DataInputStream inputStream) throws IOException {
        int b = inputStream.read();
        OpcodeTag tag = OpcodeTag.decode(b);
        switch (tag) {
        case a:
            int atomIndex = decodeInt(inputStream, b);
            if (atomIndex == 0) {
                return List.nil;
            } else {
                // TODO: atom lookup
                System.out.println("atom of index " + atomIndex);
                return Atom.of("atom" + atomIndex);
            }
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
                Term litIndex = decodeArg(inputStream);
                System.out.println("Literal at index " + litIndex);
                break;
            }
        default:
            return Integer.of(decodeInt(inputStream, b));
        }
    }

    private static int decodeInt(DataInputStream inputStream, int b) throws IOException {
        // N < 16 = 4 bits, NNNN:0:TTT
        if ((b & 0x08) == 0) {
            return b >> 4;
        }

        // N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
        if ((b & 0x10) == 0) {
            return ((b & 0b11100000) << 3) | inputStream.read();
        }

        throw new Error("Unsupported integer"); // TODO
    }

}
