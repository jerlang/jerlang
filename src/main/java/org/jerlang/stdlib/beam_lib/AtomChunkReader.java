package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;

import org.jerlang.type.Atom;

public class AtomChunkReader extends AbstractChunkReader<AtomChunk> {

    public AtomChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public AtomChunk read() throws Throwable {
        int numberOfAtoms = read4Bytes();
        AtomChunk atomChunk = new AtomChunk(chunk(), numberOfAtoms);
        for (int index = 0; index < numberOfAtoms; index++) {
            byte[] bytes = new byte[read1Byte()];
            readBytes(bytes);
            atomChunk.set(index, Atom.of(bytes));
        }
        read1Byte(); // null byte
        return atomChunk;
    }

}
