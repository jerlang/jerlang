package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;

public class ImportTableChunkReader extends AbstractChunkReader<ImportTableChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;
    private final AtomChunk atomChunk;

    public ImportTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        this.chunk = chunk;
        this.inputStream = inputStream;
        this.atomChunk = atomChunk;
    }

    public ImportTableChunk read() throws Throwable {
        ImportTableChunk importTableChunk = new ImportTableChunk(chunk.offset(), chunk.length());

        int numberOfImports = inputStream.readInt();
        while (numberOfImports-- > 0) {
            importTableChunk.add(nextFunctionSignature());
        }

        return importTableChunk;
    }

    private FunctionSignature nextFunctionSignature() throws Throwable {
        Atom module = atomChunk.atoms()[inputStream.readInt() - 1];
        Atom function = atomChunk.atoms()[inputStream.readInt() - 1];
        Integer arity = Integer.of(inputStream.readInt());
        return new FunctionSignature(module, function, arity);
    }

}
