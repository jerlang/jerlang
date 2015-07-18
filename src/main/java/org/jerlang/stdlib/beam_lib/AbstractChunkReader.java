package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.type.Term;

/**
 * A chunk reader is invoked after the chunk identifier and the length have been read.
 */
public abstract class AbstractChunkReader<T extends Chunk> extends AbstractReader {

    private final Chunk chunk;

    protected AbstractChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(inputStream);
        this.chunk = chunk;
    }

    protected Chunk chunk() {
        return chunk;
    }

    public abstract T read() throws Throwable;

    protected Term readTerm() throws Throwable {
        return new ExternalTermReader(inputStream()).read();
    }

}
