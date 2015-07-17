package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;

import org.jerlang.FunctionSignature;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.junit.Test;

public class LocalFunctionTableChunkReaderTest {

    @Test
    public void testLocalFunctionTableReader() throws Throwable {
        File file = new File("src/test/resources/fun_test.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());

        DataInputStream dis0 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk0 = new Chunk(ChunkId.ATOM, 20, 99);
        dis0.skipBytes(chunk0.offset());
        AtomChunkReader atomChunkReader = new AtomChunkReader(chunk0, dis0);
        AtomChunk atomChunk = atomChunkReader.read();

        DataInputStream dis1 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk1 = new Chunk(ChunkId.EXPT, 520, 40);
        dis1.skipBytes(chunk1.offset());
        LocalFunctionTableChunkReader localTableChunkReader = new LocalFunctionTableChunkReader(chunk1, dis1, atomChunk);
        LocalFunctionTableChunk localTableChunk = localTableChunkReader.read();
        assertNotNull(localTableChunk);
        assertNotNull(localTableChunk.locals());
        List locals = localTableChunk.locals();
        assertEquals(mfa("fun_test", "double", 1, 6), locals.head());
        locals = locals.tail();
        assertEquals(mfa("fun_test", "-t2/0-fun-0-", 1, 12), locals.head());
        locals = locals.tail();
        assertEquals(mfa("fun_test", "-t1/0-fun-0-", 1, 14), locals.head());
        assertEquals(List.nil, locals.tail());
    }

    private static FunctionSignature mfa(String m, String f, int a, int l) {
        return new FunctionSignature(Atom.of(m), Atom.of(f), Integer.of(a), Integer.of(l));
    }

}
