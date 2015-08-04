package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.OpcodeTag;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class AbstractReader {

    private static final Atom atom = Atom.of("atom");

    private DataInputStream inputStream;

    public AbstractReader(DataInputStream inputStream) {
        this.inputStream = inputStream;
    }

    protected DataInputStream inputStream() {
        return inputStream;
    }

    protected int read1Byte() throws IOException {
        return inputStream.readUnsignedByte();
    }

    protected int read2Bytes() throws IOException {
        return inputStream.readUnsignedShort();
    }

    protected int read4Bytes() throws IOException {
        return inputStream.readInt();
    }

    protected int readBytes(byte[] bytes) throws IOException {
        return inputStream.read(bytes);
    }

    protected void setInputStream(DataInputStream inputStream) {
        this.inputStream = inputStream;
    }

    protected Term decodeArg(AtomChunk atomChunk, LiteralTableChunk literalTableChunk) throws IOException {
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
        case i:
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
                Term litIndex = decodeArg(atomChunk, literalTableChunk);
                return literalTableChunk.literals().get(litIndex.toInteger().toInt());
            }
        default:
            return Tuple.of(tag.toAtom(), Integer.of(decodeInt(b)));
        }
    }

    protected int decodeInt(int b) throws IOException {
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
