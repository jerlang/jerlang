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

        DataInputStream dis0 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk0 = new Chunk(ChunkId.ATOM, 20, 75);
        dis0.skipBytes(chunk0.offset());
        AtomChunkReader atomChunkReader = new AtomChunkReader(chunk0, dis0);
        AtomChunk atomChunk = atomChunkReader.read();

        DataInputStream dis1 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk1 = new Chunk(ChunkId.LITT, 344, 33);
        dis1.skipBytes(chunk1.offset());
        LiteralTableChunkReader literalTableChunkReader = new LiteralTableChunkReader(chunk1, dis1);
        LiteralTableChunk literalTableatomChunk = literalTableChunkReader.read();

        DataInputStream dis2 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk2 = new Chunk(ChunkId.CODE, 104, 116);
        dis2.skipBytes(chunk2.offset());
        CodeChunkReader codeChunkReader = new CodeChunkReader(chunk2, dis2, atomChunk, literalTableatomChunk);
        CodeChunk codeChunk = codeChunkReader.read();
        assertNotNull(codeChunk);
        assertEquals(10, codeChunk.numberOfLabels());
        assertEquals(3, codeChunk.functions());
    }

}
