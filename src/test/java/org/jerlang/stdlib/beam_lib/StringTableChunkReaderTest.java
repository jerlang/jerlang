package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;

import org.junit.Test;

public class StringTableChunkReaderTest {

    @Test
    public void testStringTableChunkReader() throws Throwable {
        File file = new File("src/test/resources/pid.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk = new Chunk(ChunkId.STRT, 228, 0);
        dis.skipBytes(chunk.offset());
        StringTableChunkReader stringTableChunkReader = new StringTableChunkReader(chunk, dis);
        StringTableChunk stringTableChunk = stringTableChunkReader.read();
        assertNotNull(stringTableChunk);
        assertNotNull(stringTableChunk.strings());
        assertEquals(0, stringTableChunk.strings().length());
    }

}
