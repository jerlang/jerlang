package org.jerlang.stdlib.beam_lib;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;

import com.jcraft.jzlib.InflaterInputStream;

public class LiteralTableChunkReader extends AbstractChunkReader<LiteralTableChunk> {

    public LiteralTableChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public LiteralTableChunk read() throws Throwable {
        LiteralTableChunk literalTableChunk = new LiteralTableChunk(chunk());
        int uncompressedSize = read4Bytes();
        byte[] uncompressed = new byte[uncompressedSize];
        int offset = 0;
        int left = uncompressedSize;

        try (InflaterInputStream iis = new InflaterInputStream(inputStream())) {
            do {
                int decompressed = iis.read(uncompressed, offset, left);
                offset += decompressed;
                left -= decompressed;
            } while (left > 0);
        }

        setInputStream(new DataInputStream(new ByteArrayInputStream(uncompressed)));

        int literals = read4Bytes();
        while (literals-- > 0) {
            read4Bytes(); // length of literal
            literalTableChunk.add(readTerm());
        }

        return literalTableChunk;
    }
}
