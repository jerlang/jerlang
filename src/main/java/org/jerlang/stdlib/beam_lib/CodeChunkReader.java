package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.Opcode;
import org.jerlang.erts.emulator.Instruction;

public class CodeChunkReader extends AbstractChunkReader<CodeChunk> {

    public CodeChunkReader(Chunk chunk, DataInputStream inputStream) {
        super(chunk, inputStream);
    }

    public CodeChunk read() throws Throwable {
        CodeChunk codeChunk = new CodeChunk(chunk());

        checkInfoSize();
        checkVersion();
        checkMaxOpcode();
        codeChunk.setInfo(read4Bytes(), read4Bytes());

        while (codeChunk.add(nextInstruction()));

        return codeChunk;
    }

    private void check(boolean condition, String message) {
        if (condition) {
            throw new Error(message);
        }
    }

    private void checkInfoSize() throws IOException {
        int infoSize = read4Bytes();
        check(infoSize != 16, "Unexpected info-size header value: " + infoSize);
    }

    private void checkVersion() throws IOException {
        int version = read4Bytes();
        check(version != 0, "Unsupported instruction set version: " + version);
    }

    private void checkMaxOpcode() throws IOException {
        int maxOpcode = read4Bytes();
        check(maxOpcode > Opcode.max(), "maxOpcode > " + Opcode.max());
    }

    private Instruction nextInstruction() throws Throwable {
        Opcode opcode = Opcode.decode(read1Byte());
        return InstructionReader.read(opcode, inputStream());
    }

}
