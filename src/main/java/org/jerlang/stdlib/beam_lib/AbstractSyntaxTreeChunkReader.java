package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

public class AbstractSyntaxTreeChunkReader extends AbstractChunkReader<AbstractSyntaxTreeChunk> {

    public AbstractSyntaxTreeChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public AbstractSyntaxTreeChunk read() throws Throwable {
        return new AbstractSyntaxTreeChunk(chunk(), readTerm());
    }

}
