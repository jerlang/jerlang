package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

public class StringTableChunkReader extends AbstractChunkReader<StringTableChunk> {

    public StringTableChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public StringTableChunk read() throws Throwable {
        byte[] bytes = new byte[chunk().length()];
        readBytes(bytes);
        return new StringTableChunk(chunk(), bytes);
    }

}
