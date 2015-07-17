package org.jerlang.stdlib.beam_lib;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.erts.ExternalTermFormatTag;
import org.jerlang.type.Str;
import org.jerlang.type.Term;

import com.jcraft.jzlib.InflaterInputStream;

public class LiteralTableChunkReader extends AbstractChunkReader<LiteralTableChunk> {

    private final Chunk chunk;
    private DataInputStream inputStream;

    public LiteralTableChunkReader(Chunk chunk, DataInputStream inputStream) {
        this.chunk = chunk;
        this.inputStream = inputStream;
    }

    public LiteralTableChunk read() throws Throwable {
        LiteralTableChunk literalTableChunk = new LiteralTableChunk(chunk.offset(), chunk.length());
        int uncompressedSize = inputStream.readInt();
        byte[] uncompressed = new byte[uncompressedSize];
        int offset = 0;
        int left = uncompressedSize;

        try (InflaterInputStream iis = new InflaterInputStream(inputStream)) {
            do {
                int decompressed = iis.read(uncompressed, offset, left);
                offset += decompressed;
                left -= decompressed;
            } while (left > 0);
        }

        inputStream = new DataInputStream(new ByteArrayInputStream(uncompressed));

        int literals = inputStream.readInt();
        while (literals-- > 0) {
            literalTableChunk.add(nextLiteral());
        }

        return literalTableChunk;
    }

    private Term nextLiteral() throws Throwable {
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
