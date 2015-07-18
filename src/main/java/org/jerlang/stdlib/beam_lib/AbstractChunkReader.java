package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.erts.ExternalTermFormatTag;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

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

    protected int readBytes(byte[] bytes) throws IOException {
        return inputStream.read(bytes);
    }

    protected void setInputStream(DataInputStream inputStream) {
        this.inputStream = inputStream;
    }

    protected Term nextLiteral() throws Throwable {
        inputStream.readInt(); // length of literal record
        int version = inputStream.read();
        if (version != 131) {
            // When messages are passed between connected nodes and a
            // distribution header is used, the first byte containing
            // the version number (131) is omitted from the terms that
            // follow the distribution header. This since the version
            // number is implied by the version number in the dist. header.
            throw new Error("Version 131 not found");
        }
        ExternalTermFormatTag tag = ExternalTermFormatTag.of(inputStream.read());
        switch (tag) {
        case STRING_EXT:
            return nextString();
        default:
            // TODO: we could also skip by (length - 2) bytes
            throw new Error("Tag " + tag + " not supported yet");
        }
    }

    private Str nextString() throws IOException {
        int len = inputStream.readUnsignedShort();
        byte[] bytes = new byte[len];
        inputStream.read(bytes);
        Str s = Str.of(new String(bytes));
        return s;
    }

}
