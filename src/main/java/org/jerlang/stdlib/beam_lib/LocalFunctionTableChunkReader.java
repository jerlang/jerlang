package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;

public class LocalFunctionTableChunkReader extends AbstractChunkReader<LocalFunctionTableChunk> {

    private final AtomChunk atomChunk;

    public LocalFunctionTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        super(chunk, inputStream);
        this.atomChunk = atomChunk;
    }

    public LocalFunctionTableChunk read() throws Throwable {
        LocalFunctionTableChunk localFunctionTableChunk = new LocalFunctionTableChunk(chunk());

        int numberOfLocalFunctions = read4Bytes();
        while (numberOfLocalFunctions-- > 0) {
            localFunctionTableChunk.add(nextFunctionSignature());
        }

        return localFunctionTableChunk;
    }

    private FunctionSignature nextFunctionSignature() throws Throwable {
        Atom module = atomChunk.atoms()[0];
        Atom function = atomChunk.atoms()[read4Bytes() - 1];
        Integer arity = Integer.of(read4Bytes());
        Integer label = Integer.of(read4Bytes());
        return new FunctionSignature(module, function, arity, label);
    }

}
