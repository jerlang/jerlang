package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;

public class ExportTableChunkReader extends AbstractChunkReader<ExportTableChunk> {

    private final AtomChunk atomChunk;

    public ExportTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        super(chunk, inputStream);
        this.atomChunk = atomChunk;
    }

    public ExportTableChunk read() throws Throwable {
        ExportTableChunk exportTableChunk = new ExportTableChunk(chunk());

        int numberOfImports = read4Bytes();
        while (numberOfImports-- > 0) {
            exportTableChunk.add(nextFunctionSignature());
        }

        return exportTableChunk;
    }

    private FunctionSignature nextFunctionSignature() throws Throwable {
        Atom module = atomChunk.atoms()[0];
        Atom function = atomChunk.atoms()[read4Bytes() - 1];
        Integer arity = Integer.of(read4Bytes());
        Integer label = Integer.of(read4Bytes());
        return new FunctionSignature(module, function, arity, label);
    }

}
