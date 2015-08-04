package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;
import java.util.List;

import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.junit.Test;

public class LiteralTableChunkReaderTest {

    @Test
    public void testLiteralTableChunkReader() throws Throwable {
        File file = new File("src/test/resources/fun_test.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());

        DataInputStream dis0 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk0 = new Chunk(ChunkId.LITT, 480, 29);
        dis0.skipBytes(chunk0.offset());
        LiteralTableChunkReader literalTableChunkReader = new LiteralTableChunkReader(chunk0, dis0);
        LiteralTableChunk literalTableChunk = literalTableChunkReader.read();
        assertNotNull(literalTableChunk);
        assertNotNull(literalTableChunk.literals());
        List<Term> literals = literalTableChunk.literals();
        assertEquals(Str.of(new String(new byte[] { 1, 2, 3, 4, 5 })), literals.get(0));
        assertEquals(1, literals.size());
    }

}
