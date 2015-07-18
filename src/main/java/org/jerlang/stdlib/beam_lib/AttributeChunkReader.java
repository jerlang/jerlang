package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

public class AttributeChunkReader extends AbstractChunkReader<AttributeChunk> {

    public AttributeChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public AttributeChunk read() throws Throwable {
        return new AttributeChunk(chunk(), readTerm());
    }

}
