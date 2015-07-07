package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

public class AtomChunkReader extends AbstractChunkReader<AtomChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;

    public AtomChunkReader(Chunk chunk, DataInputStream inputStream) {
        this.chunk = chunk;
        this.inputStream = inputStream;
    }

    public AtomChunk read() throws IOException {
        int atoms = inputStream.readInt();
        for (int index = 0; index < atoms; index++) {
            int size = inputStream.read();
            byte[] bytes = new byte[size];
            inputStream.read(bytes);
            System.out.println("Atom: " + new String(bytes));
        }
        inputStream.read(); // null byte
        return null;
    }

}
