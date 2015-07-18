package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;

import org.jerlang.erts.ExternalTermFormatTag;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * A chunk reader is invoked after the chunk identifier and the length have been read.
 */
public abstract class AbstractChunkReader<T extends Chunk> {

    private final Chunk chunk;
    private DataInputStream inputStream;

    protected AbstractChunkReader(Chunk chunk, DataInputStream inputStream) {
        this.chunk = chunk;
        this.inputStream = inputStream;
    }

    protected Chunk chunk() {
        return chunk;
    }

    protected DataInputStream inputStream() {
        return inputStream;
    }

    public abstract T read() throws Throwable;

    protected int read1Byte() throws IOException {
        return inputStream.readUnsignedByte();
    }

    protected int read2Bytes() throws IOException {
        return inputStream.readUnsignedShort();
    }

    protected int read4Bytes() throws IOException {
        return inputStream.readInt();
    }

    private Atom readAtom() throws IOException {
        byte[] bytes = new byte[read2Bytes()];
        readBytes(bytes);
        return Atom.of(bytes);
    }

    protected int readBytes(byte[] bytes) throws IOException {
        return inputStream.read(bytes);
    }

    private List readList() throws Throwable {
        int length = read4Bytes();
        List list = new List();
        while (length-- > 0) {
            list = new List(readTerm(), list);
        }
        return list;
    }

    private Term readSmallBig() throws IOException {
        int n = read1Byte();
        int sign = read1Byte();
        byte[] d = new byte[n];
        readBytes(d);
        Collections.reverse(Arrays.asList(d));
        return new Integer(new BigInteger(d));
    }

    private Term readSmallTuple() throws Throwable {
        int arity = read1Byte();
        Term[] terms = new Term[arity];
        for (int index = 0; index < arity; index++) {
            terms[index] = readTerm();
        }
        return Tuple.of(terms);
    }

    private Str readString() throws IOException {
        int len = inputStream.readUnsignedShort();
        byte[] bytes = new byte[len];
        inputStream.read(bytes);
        Str s = Str.of(new String(bytes));
        return s;
    }

    protected Term readTerm() throws Throwable {
        int versionOrTag = read1Byte();
        int tagId;
        if (versionOrTag == 131) {
            tagId = read1Byte();
        } else {
            tagId = versionOrTag;
        }
        ExternalTermFormatTag tag = ExternalTermFormatTag.of(tagId);
        switch (tag) {
        case ATOM_EXT:
            return readAtom();
        case INTEGER_EXT:
            return Integer.of(read4Bytes());
        case LIST_EXT:
            return readList();
        case SMALL_BIG_EXT:
            return readSmallBig();
        case SMALL_INTEGER_EXT:
            return Integer.of(read1Byte());
        case SMALL_TUPLE_EXT:
            return readSmallTuple();
        case STRING_EXT:
            return readString();
        default:
            throw new Error("Tag " + tag + " not supported yet");
        }
    }

    protected void setInputStream(DataInputStream inputStream) {
        this.inputStream = inputStream;
    }

}
