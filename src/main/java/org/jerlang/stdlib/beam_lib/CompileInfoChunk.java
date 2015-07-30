package org.jerlang.stdlib.beam_lib;

import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 * = The Compile Info Chunk
 *
 * The Compile Info chunk contains information about the compilation,
 * for example, the name of the source file name.
 */
public class CompileInfoChunk extends Chunk {

    private final List compile_info;

    public CompileInfoChunk(Chunk chunk, Term compile_info) {
        super(ChunkId.CINF, chunk);
        this.compile_info = compile_info.toList();
    }

    public List compile_info() {
        return compile_info;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("{compile_info,[");
        List list = compile_info;
        while (list.length() > 0) {
            stringBuilder.append(list.head()).append(',');
            list = list.tail();
        }
        if (compile_info.length() > 0) {
            stringBuilder.setLength(stringBuilder.length() - 1);
        }
        stringBuilder.append("]}");
        return stringBuilder.toString();
    }

}
