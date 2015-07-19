package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;

import org.jerlang.erts.emulator.Instruction;
import org.junit.Test;

public class CodeChunkReaderTest {

    @Test
    public void testCodeChunkReader() throws Throwable {
        File file = new File("src/test/resources/pid.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());

        DataInputStream dis0 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk0 = new Chunk(ChunkId.ATOM, 20, 75);
        dis0.skipBytes(chunk0.offset());
        AtomChunkReader atomChunkReader = new AtomChunkReader(chunk0, dis0);
        AtomChunk atomChunk = atomChunkReader.read();

        DataInputStream dis1 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk1 = new Chunk(ChunkId.CODE, 104, 116);
        dis1.skipBytes(chunk1.offset());
        CodeChunkReader codeChunkReader = new CodeChunkReader(chunk1, dis1, atomChunk);
        CodeChunk codeChunk = codeChunkReader.read();
        assertNotNull(codeChunk);
        assertEquals(10, codeChunk.labels());
        assertEquals(3, codeChunk.functions());
        for (Instruction i : codeChunk.instructions()) {
            System.out.println(i);
        }
    }

}
