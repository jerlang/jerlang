package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;

public class ImportTableChunkReader extends AbstractChunkReader<ImportTableChunk> {

    private final AtomChunk atomChunk;

    public ImportTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        super(chunk, inputStream);
        this.atomChunk = atomChunk;
    }

    public ImportTableChunk read() throws Throwable {
        ImportTableChunk importTableChunk = new ImportTableChunk(chunk());

        int numberOfImports = read4Bytes();
        while (numberOfImports-- > 0) {
            importTableChunk.add(nextFunctionSignature());
        }

        return importTableChunk;
    }

    private FunctionSignature nextFunctionSignature() throws Throwable {
        Atom module = atomChunk.atoms()[read4Bytes() - 1];
        Atom function = atomChunk.atoms()[read4Bytes() - 1];
        Integer arity = Integer.of(read4Bytes());
        return new FunctionSignature(module, function, arity);
    }

}
