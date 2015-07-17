package org.jerlang.stdlib.beam_lib;

import java.util.ArrayList;

import org.jerlang.FunctionSignature;
import org.jerlang.type.List;

/**
 * = Local Function Table Chunk
 *
 * The Local Function Table Chunk is used to define methods that this module
 * defines, but does not export.
 *
 * The data format is equal to the data format of the Export Table Chunk.
 */
public class LocalFunctionTableChunk extends Chunk {

    private final ArrayList<FunctionSignature> locals;

    public LocalFunctionTableChunk(int offset, int length) {
        super(ChunkId.LOCT, offset, length);
        locals = new ArrayList<>();
    }

    public void add(FunctionSignature functionSignature) {
        locals.add(functionSignature);
    }

    public List locals() {
        return List.of(locals);
    }

}
