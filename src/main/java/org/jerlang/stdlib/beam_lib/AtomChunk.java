package org.jerlang.stdlib.beam_lib;

public class AtomChunk extends Chunk {

    public AtomChunk(int offset, int length) {
        super(ChunkId.ATOM, offset, length);
    }

}
