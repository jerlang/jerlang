package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Integer;
import org.jerlang.type.Tuple;

public class Chunk {

    private ChunkId id;
    private int offset;
    private int length;

    public Chunk(ChunkId identifier, int offset, int length) {
        this.id = identifier;
        this.offset = offset;
        this.length = length;
    }

    public ChunkId id() {
        return id;
    }

    public int offset() {
        return offset;
    }

    public int length() {
        return length;
    }

    public Tuple asTuple() {
        return Tuple.of(id.toStr(), Integer.of(offset), Integer.of(length));
    }

}
