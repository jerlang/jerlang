package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

public class FunctionTableChunkReader extends AbstractChunkReader<FunctionTableChunk> {

    private final AtomChunk atomChunk;

    public FunctionTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        super(chunk, inputStream);
        this.atomChunk = atomChunk;
    }

    public FunctionTableChunk read() throws Throwable {
        FunctionTableChunk functionTableChunk = new FunctionTableChunk(chunk());

        int numberOfLambdas = read4Bytes();
        while (numberOfLambdas-- > 0) {
            functionTableChunk.add(nextLambdaInfo());
        }

        return functionTableChunk;
    }

    private LambdaInfo nextLambdaInfo() throws Throwable {
        return new LambdaInfo(
            read4Bytes(),
            read4Bytes(),
            read4Bytes(),
            read4Bytes(),
            read4Bytes(),
            read4Bytes(),
            read4Bytes(),
            atomChunk);
    }

}
