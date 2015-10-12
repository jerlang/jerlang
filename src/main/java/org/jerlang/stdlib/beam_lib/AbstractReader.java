package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;
import java.math.BigInteger;

import org.jerlang.OpcodeTag;
import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

public class AbstractReader {

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

    protected long read8Bytes() throws IOException {
        return inputStream.readLong();
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
            int atomIndex = decodeInt(b).toInt();
            if (atomIndex == 0) {
                return List.nil;
            } else {
                return atomChunk.atoms()[atomIndex - 1];
                //return Tuple.of(atom, atomChunk.atoms()[atomIndex - 1]);
            }
        case i:
        case u:
            return decodeInt(b, tag);
        case z:
            if ((b & 0x08) != 0) {
                throw new Error("invalid extended tag: " + b);
            }
            switch (b >> 4) {
            case 0:
                throw new Error("decode float not implemented yet");
            case 1:
                return decodeList(atomChunk, literalTableChunk);
            case 2:
                // float register
                return Tuple.of(Atom.of("fr"), Integer.of(read1Byte()));
            case 3:
                return decodeAllocationList();
            case 4:
                Term litIndex = decodeArg(atomChunk, literalTableChunk);
                return literalTableChunk.literals().get(litIndex.toInteger().toInt());
            default:
                throw new Error("unsupported z value: " + (b >> 4));
            }
        default:
            return Tuple.of(tag.toAtom(), decodeInt(b));
        }
    }

    private Term decodeList(AtomChunk atomChunk, LiteralTableChunk literalTableChunk) throws IOException {
        List list = List.nil;
        int elements = decodeInt(read1Byte()).toInt();
        while (elements-- > 0) {
            list = new List(decodeArg(atomChunk, literalTableChunk), list);
        }
        return Lists.reverse(list);
    }

    private Term decodeAllocationList() throws IOException {
        List allocationList = List.nil;
        int elements = decodeInt(read1Byte()).toInt();
        while (elements-- > 0) {
            int typ = decodeInt(read1Byte()).toInt();
            int val = decodeInt(read1Byte()).toInt();
            switch (typ) {
            case 0:
                allocationList = new List(Tuple.of(Atom.of("words"), Integer.of(val)), allocationList);
                break;
            case 1:
                allocationList = new List(Tuple.of(Atom.of("floats"), Integer.of(val)), allocationList);
                break;
            case 2:
                allocationList = new List(Tuple.of(Atom.of("literal"), Integer.of(val)), allocationList);
                break;
            }
        }
        return allocationList;
    }

    protected Integer decodeInt(int b) throws IOException {
        return decodeInt(b, OpcodeTag.i);
    }

    protected Integer decodeInt(int b, OpcodeTag opcodeTag) throws IOException {
        // N < 16 = 4 bits, NNNN:0:TTT
        if ((b & 0x08) == 0) {
            return Integer.of(b >> 4);
        }

        // N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
        if ((b & 0x10) == 0) {
            return Integer.of(((b & 0b11100000) << 3) | read1Byte());
        }

        // Bignum
        int numBytes = decodeIntLength(b);
        byte[] bytes = new byte[numBytes];
        readBytes(bytes);

        BigInteger n = BigInteger.ZERO;
        for (byte x : bytes) {
            n = n.shiftLeft(8);
            n = n.or(BigInteger.valueOf(x));
        }

        // Negative number
        if (bytes[0] > 127 && opcodeTag == OpcodeTag.i) {
            n = n.subtract(BigInteger.ONE.shiftLeft(numBytes));
        }

        return new Integer(n);
    }

    private int decodeIntLength(int b) throws IOException {
        int len = b >>> 5;
        if (len == 7) {
            Term term = decodeArg(null, null);
            if (term instanceof Integer) {
                return term.toInteger().toInt() + 9;
            } else {
                throw new Error("Weird bignum sublength");
            }
        } else {
            return len + 2;
        }
    }

}
