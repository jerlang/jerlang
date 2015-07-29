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

    public LocalFunctionTableChunk(Chunk chunk) {
        super(ChunkId.LOCT, chunk);
        locals = new ArrayList<>();
    }

    public void add(FunctionSignature functionSignature) {
        locals.add(functionSignature);
    }

    public List locals() {
        return List.of(locals);
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("{local_functions_chunk,[");
        for (FunctionSignature s : locals) {
            stringBuilder.append(s).append(',');
        }
        if (locals.size() > 0) {
            stringBuilder.setLength(stringBuilder.length() - 1);
        }
        stringBuilder.append("]}");
        return stringBuilder.toString();
    }

}
