package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

public class FunctionTableChunkReader extends AbstractChunkReader<FunctionTableChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;
    private final AtomChunk atomChunk;

    public FunctionTableChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk) {
        this.chunk = chunk;
        this.inputStream = inputStream;
        this.atomChunk = atomChunk;
    }

    public FunctionTableChunk read() throws Throwable {
        FunctionTableChunk functionTableChunk = new FunctionTableChunk(chunk.offset(), chunk.length());

        int numberOfLambdas = inputStream.readInt();
        while (numberOfLambdas-- > 0) {
            functionTableChunk.add(nextLambdaInfo());
        }

        return functionTableChunk;
    }

    private LambdaInfo nextLambdaInfo() throws Throwable {
        return new LambdaInfo(
            inputStream.readInt(),
            inputStream.readInt(),
            inputStream.readInt(),
            inputStream.readInt(),
            inputStream.readInt(),
            inputStream.readInt(),
            inputStream.readInt(),
            atomChunk);
    }

}
