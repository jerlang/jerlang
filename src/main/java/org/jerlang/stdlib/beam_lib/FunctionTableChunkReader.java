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

        int index = 0;
        int numberOfLambdas = read4Bytes();
        while (numberOfLambdas-- > 0) {
            functionTableChunk.add(nextLambdaInfo(index++));
        }

        return functionTableChunk;
    }

    private LambdaInfo nextLambdaInfo(int index) throws Throwable {
        return new LambdaInfo(
            read4Bytes(), // function atom id
            read4Bytes(), // arity
            read4Bytes(), // label
            read4Bytes(), // index
            read4Bytes(), // num-free
            read4Bytes(), // old-unique
            index, // old-index
            atomChunk);
    }

}
