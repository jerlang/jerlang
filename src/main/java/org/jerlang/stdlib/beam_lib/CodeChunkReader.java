package org.jerlang.stdlib.beam_lib;

import java.io.DataInputStream;
import java.io.IOException;

import org.jerlang.Opcode;
import org.jerlang.erts.emulator.Instruction;

public class CodeChunkReader extends AbstractChunkReader<CodeChunk> {

    private final InstructionReader instructionReader;

    public CodeChunkReader(Chunk chunk, DataInputStream inputStream, AtomChunk atomChunk,
        LiteralTableChunk literalTableChunk) {
        super(chunk, inputStream);
        instructionReader = new InstructionReader(inputStream, atomChunk, literalTableChunk);
    }

    public CodeChunk read() throws Throwable {
        CodeChunk codeChunk = new CodeChunk(chunk());

        checkInfoSize(read4Bytes());
        checkVersion(read4Bytes());
        checkMaxOpcode(read4Bytes());
        codeChunk.setInfo(read4Bytes(), read4Bytes());

        int count = 0;
        while (codeChunk.add(nextInstruction(count++)));

        return codeChunk;
    }

    private void check(boolean condition, String message) {
        if (condition) {
            throw new Error(message);
        }
    }

    private void checkInfoSize(int infoSize) throws IOException {
        check(infoSize != 16, "Unexpected info-size header value: " + infoSize);
    }

    private void checkVersion(int version) throws IOException {
        check(version != 0, "Unsupported instruction set version: " + version);
    }

    private void checkMaxOpcode(int maxOpcode) throws IOException {
        check(maxOpcode > Opcode.max(), "maxOpcode > " + Opcode.max());
    }

    private Instruction nextInstruction(int count) throws Throwable {
        Opcode opcode = Opcode.decode(read1Byte());
        return instructionReader.read(count, opcode);
    }

}
