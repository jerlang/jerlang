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

public class ImportChunkReaderTest {

    @Test
    public void testImportChunkReader() throws Throwable {
        File file = new File("src/test/resources/pid.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());

        DataInputStream dis0 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk0 = new Chunk(ChunkId.ATOM, 20, 75);
        dis0.skipBytes(chunk0.offset());
        AtomChunkReader atomChunkReader = new AtomChunkReader(chunk0, dis0);
        AtomChunk atomChunk = atomChunkReader.read();

        DataInputStream dis1 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk1 = new Chunk(ChunkId.IMPT, 236, 52);
        dis1.skipBytes(chunk1.offset());
        ImportTableChunkReader importTableChunkReader = new ImportTableChunkReader(chunk1, dis1, atomChunk);
        ImportTableChunk importTableChunk = importTableChunkReader.read();
        assertNotNull(importTableChunk);
        assertNotNull(importTableChunk.imports());
        List imports = importTableChunk.imports();
        assertEquals(mfa("erlang", "get_module_info", 2), imports.head());
        imports = imports.tail();
        assertEquals(mfa("erlang", "get_module_info", 1), imports.head());
        imports = imports.tail();
        assertEquals(mfa("io", "format", 2), imports.head());
        imports = imports.tail();
        assertEquals(mfa("erlang", "self", 0), imports.head());
        imports = imports.tail();
        assertEquals(null, imports.tail());
    }

    private static FunctionSignature mfa(String m, String f, int a) {
        return new FunctionSignature(Atom.of(m), Atom.of(f), Integer.of(a));
    }

}
