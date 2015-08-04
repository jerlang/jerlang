package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class FunctionTableChunkReaderTest {

    @Test
    public void testFunctionTableChunkReader() throws Throwable {
        File file = new File("src/test/resources/fun_test.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());

        DataInputStream dis0 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk0 = new Chunk(ChunkId.ATOM, 20, 99);
        dis0.skipBytes(chunk0.offset());
        AtomChunkReader atomChunkReader = new AtomChunkReader(chunk0, dis0);
        AtomChunk atomChunk = atomChunkReader.read();

        DataInputStream dis1 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk1 = new Chunk(ChunkId.FUNT, 420, 52);
        dis1.skipBytes(chunk1.offset());
        FunctionTableChunkReader functionTableChunkReader = new FunctionTableChunkReader(chunk1, dis1, atomChunk);
        FunctionTableChunk functionTableChunk = functionTableChunkReader.read();
        assertNotNull(functionTableChunk);
        assertNotNull(functionTableChunk.lambdas());
        List lambdas = functionTableChunk.lambdas();
        assertEquals(li(1, "-t1/0-fun-0-", 1, 0, 74200244, 1281979476, 29), lambdas.head());
        lambdas = lambdas.tail();
        assertEquals(li(12, "fun_test", 14, 0, 0, 74200244, 11), lambdas.head());
        assertEquals(List.nil, lambdas.tail());
    }

    private static Tuple li(int idx, String m, int a, int l, int i, int nf, int ou) {
        return Tuple.of(
            Integer.of(idx),
            Tuple.of(
                Atom.of(m),
                Integer.of(a), // arity
                Integer.of(l), // label
                Integer.of(i), // index
                Integer.of(nf), // num-free
                Integer.of(ou) // old-unique
                ));
    }

}
