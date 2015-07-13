package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;

import org.junit.Test;

public class AtomChunkReaderTest {

    @Test
    public void testAtomChunkReader() throws Throwable {
        File file = new File("src/test/resources/pid.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk = new Chunk(ChunkId.ATOM, 20, 75);
        dis.skipBytes(chunk.offset());
        AtomChunkReader atomChunkReader = new AtomChunkReader(chunk, dis);
        AtomChunk atomChunk = atomChunkReader.read();
        assertNotNull(atomChunk);
        assertNotNull(atomChunk.atoms());
        assertEquals(9, atomChunk.atoms().length);
        assertEquals("pid", atomChunk.atoms()[0].toString());
        assertEquals("loop", atomChunk.atoms()[1].toString());
        assertEquals("who_are_you", atomChunk.atoms()[2].toString());
        assertEquals("erlang", atomChunk.atoms()[3].toString());
        assertEquals("self", atomChunk.atoms()[4].toString());
        assertEquals("io", atomChunk.atoms()[5].toString());
        assertEquals("format", atomChunk.atoms()[6].toString());
        assertEquals("module_info", atomChunk.atoms()[7].toString());
        assertEquals("get_module_info", atomChunk.atoms()[8].toString());
    }

}
