package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;

public class LocalFunctionTableChunkReader extends AbstractChunkReader<LocalFunctionTableChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;
    private final AtomChunk atomChunk;

    public LocalFunctionTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        this.chunk = chunk;
        this.inputStream = inputStream;
        this.atomChunk = atomChunk;
    }

    public LocalFunctionTableChunk read() throws Throwable {
        LocalFunctionTableChunk localFunctionTableChunk = new LocalFunctionTableChunk(chunk.offset(), chunk.length());

        int numberOfLocalFunctions = inputStream.readInt();
        while (numberOfLocalFunctions-- > 0) {
            localFunctionTableChunk.add(nextFunctionSignature());
        }

        return localFunctionTableChunk;
    }

    private FunctionSignature nextFunctionSignature() throws Throwable {
        Atom module = atomChunk.atoms()[0];
        Atom function = atomChunk.atoms()[inputStream.readInt() - 1];
        Integer arity = Integer.of(inputStream.readInt());
        Integer label = Integer.of(inputStream.readInt());
        return new FunctionSignature(module, function, arity, label);
    }

}
