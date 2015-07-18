package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.type.Atom;

public class AtomChunkReader extends AbstractChunkReader<AtomChunk> {

    public AtomChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public AtomChunk read() throws Throwable {
        AtomChunk atomChunk = new AtomChunk(chunk());
        int numberOfAtoms = read4Bytes();
        atomChunk.init(numberOfAtoms);
        for (int index = 0; index < numberOfAtoms; index++) {
            int size = read1Byte();
            byte[] bytes = new byte[size];
            readBytes(bytes);
            atomChunk.set(index, Atom.of(bytes));
        }
        read1Byte(); // null byte
        return atomChunk;
    }

}
