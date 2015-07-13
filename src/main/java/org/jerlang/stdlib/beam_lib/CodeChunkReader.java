package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.Opcode;
import org.jerlang.erts.emulator.Instruction;

public class CodeChunkReader extends AbstractChunkReader<CodeChunk> {

    private final Chunk chunk;
    private final DataInputStream inputStream;

    public CodeChunkReader(Chunk chunk, DataInputStream inputStream) {
        this.chunk = chunk;
        this.inputStream = inputStream;
    }

    public CodeChunk read() throws Throwable {
        CodeChunk codeChunk = new CodeChunk(chunk.offset(), chunk.length());

        checkInfoSize();
        checkVersion();
        checkMaxOpcode();
        codeChunk.setInfo(inputStream.readInt(), inputStream.readInt());

        while (codeChunk.add(nextInstruction()));

        return codeChunk;
    }

    private void check(boolean condition, String message) {
        if (condition) {
            throw new Error(message);
        }
    }

    private void checkInfoSize() throws IOException {
        int infoSize = inputStream.readInt();
        check(infoSize != 16, "Unexpected info-size header value: " + infoSize);
    }

    private void checkVersion() throws IOException {
        int version = inputStream.readInt();
        check(version != 0, "Unsupported instruction set version: " + version);
    }

    private void checkMaxOpcode() throws IOException {
        int maxOpcode = inputStream.readInt();
        check(maxOpcode > Opcode.max(), "maxOpcode > " + Opcode.max());
    }

    private Instruction nextInstruction() throws Throwable {
        Opcode opcode = Opcode.decode(inputStream.read());
        return InstructionReader.read(opcode, inputStream);
    }

}
