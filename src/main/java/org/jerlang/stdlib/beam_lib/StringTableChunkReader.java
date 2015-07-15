package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

public class StringTableChunkReader extends AbstractChunkReader<StringTableChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;

    public StringTableChunkReader(Chunk chunk, DataInputStream inputStream) {
        this.chunk = chunk;
        this.inputStream = inputStream;
    }

    public StringTableChunk read() throws Throwable {
        byte[] bytes = new byte[chunk.length()];
        inputStream.read(bytes);
        return new StringTableChunk(chunk.offset(), chunk.length(), bytes);
    }

}
