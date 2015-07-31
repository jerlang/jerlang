package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.type.List;

public class AbstractSyntaxTreeChunkReader extends AbstractChunkReader<AbstractSyntaxTreeChunk> {

    public AbstractSyntaxTreeChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public AbstractSyntaxTreeChunk read() throws Throwable {
        if (chunk().length() > 0) {
            return new AbstractSyntaxTreeChunk(chunk(), readTerm());
        } else {
            return new AbstractSyntaxTreeChunk(chunk(), List.nil);
        }
    }

}
