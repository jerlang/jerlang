package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.type.Atom;

public class AtomChunkReader extends AbstractChunkReader<AtomChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;

    public AtomChunkReader(Chunk chunk, DataInputStream inputStream) {
        this.chunk = chunk;
        this.inputStream = inputStream;
    }

    public AtomChunk read() throws IOException {
        AtomChunk atomChunk = new AtomChunk(chunk.offset(), chunk.length());
        int numberOfAtoms = inputStream.readInt();
        atomChunk.init(numberOfAtoms);
        for (int index = 0; index < numberOfAtoms; index++) {
            int size = inputStream.read();
            byte[] bytes = new byte[size];
            inputStream.read(bytes);
            atomChunk.set(index, Atom.of(bytes));
        }
        inputStream.read(); // null byte
        return atomChunk;
    }

}
