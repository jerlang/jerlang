package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

public class AbstractReader {

    private DataInputStream inputStream;

    public AbstractReader(DataInputStream inputStream) {
        this.inputStream = inputStream;
    }

    protected DataInputStream inputStream() {
        return inputStream;
    }

    protected int read1Byte() throws IOException {
        return inputStream.readUnsignedByte();
    }

    protected int read2Bytes() throws IOException {
        return inputStream.readUnsignedShort();
    }

    protected int read4Bytes() throws IOException {
        return inputStream.readInt();
    }

    protected int readBytes(byte[] bytes) throws IOException {
        return inputStream.read(bytes);
    }

    protected void setInputStream(DataInputStream inputStream) {
        this.inputStream = inputStream;
    }

}
