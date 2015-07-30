package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

public class CompileInfoChunkReader extends AbstractChunkReader<CompileInfoChunk> {

    public CompileInfoChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public CompileInfoChunk read() throws Throwable {
        return new CompileInfoChunk(chunk(), readTerm());
    }

}
