package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.Integer;
import org.jerlang.type.Tuple;

public class Chunk {

    private final ChunkId id;
    private final int offset;
    private final int length;

    public Chunk(ChunkId expectedChunkId, Chunk chunk) {
        this(chunk.id, chunk.offset, chunk.length);
        if (id != expectedChunkId) {
            throw new Error("Unexpected chunk id: " + id);
        }
    }

    public Chunk(ChunkId id, int offset, int length) {
        this.id = id;
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
