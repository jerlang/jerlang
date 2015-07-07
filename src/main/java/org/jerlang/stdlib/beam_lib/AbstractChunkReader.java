package org.jerlang.stdlib.beam_lib;

import java.io.IOException;

/**
 * A chunk reader is invoked after the chunk identifier and the length have been read.
 */
public abstract class AbstractChunkReader<T extends Chunk> {

    public abstract T read() throws IOException;

}
