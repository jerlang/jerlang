package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;

public class ExportTableChunkReader extends AbstractChunkReader<ExportTableChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;
    private final AtomChunk atomChunk;

    public ExportTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        this.chunk = chunk;
        this.inputStream = inputStream;
        this.atomChunk = atomChunk;
    }

    public ExportTableChunk read() throws Throwable {
        ExportTableChunk exportTableChunk = new ExportTableChunk(chunk.offset(), chunk.length());

        int numberOfImports = inputStream.readInt();
        while (numberOfImports-- > 0) {
            exportTableChunk.add(nextFunctionSignature());
        }

        return exportTableChunk;
    }

    private FunctionSignature nextFunctionSignature() throws Throwable {
        Atom module = atomChunk.atoms()[0];
        Atom function = atomChunk.atoms()[inputStream.readInt() - 1];
        Integer arity = Integer.of(inputStream.readInt());
        Integer label = Integer.of(inputStream.readInt());
        return new FunctionSignature(module, function, arity, label);
    }

}
