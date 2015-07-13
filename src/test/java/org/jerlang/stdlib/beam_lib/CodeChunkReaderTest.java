package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;

import org.junit.Test;

public class CodeChunkReaderTest {

    @Test
    public void testCodeChunkReader() throws Throwable {
        File file = new File("src/test/resources/pid.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk = new Chunk(ChunkId.CODE, 104, 116);
        dis.skipBytes(chunk.offset());
        CodeChunkReader codeChunkReader = new CodeChunkReader(chunk, dis);
        CodeChunk codeChunk = codeChunkReader.read();
        assertNotNull(codeChunk);
        assertEquals(10, codeChunk.labels());
        assertEquals(3, codeChunk.functions());
    }

}
