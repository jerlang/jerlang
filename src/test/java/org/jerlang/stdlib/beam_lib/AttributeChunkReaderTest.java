package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.math.BigInteger;
import java.nio.file.Files;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class AttributeChunkReaderTest {

    @Test
    public void testAtomChunkReader() throws Throwable {
        File file = new File("src/test/resources/fun_test.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());
        DataInputStream dis = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk = new Chunk(ChunkId.ATTR, 568, 40);
        dis.skipBytes(chunk.offset());
        AttributeChunkReader reader = new AttributeChunkReader(chunk, dis);
        AttributeChunk attributeChunk = reader.read();
        assertNotNull(attributeChunk);
        assertNotNull(attributeChunk.attributes());
        List list = attributeChunk.attributes();
        assertEquals(Tuple.of(
            Atom.of("vsn"),
            List.of(
                new Integer(new BigInteger("42628055827663549810906328445318760077"))
                )), list.head());
    }

}
