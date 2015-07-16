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

public class ExportChunkReaderTest {

    @Test
    public void testExportChunkReader() throws Throwable {
        File file = new File("src/test/resources/pid.beam");
        byte[] bytes = Files.readAllBytes(file.toPath());

        DataInputStream dis0 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk0 = new Chunk(ChunkId.ATOM, 20, 75);
        dis0.skipBytes(chunk0.offset());
        AtomChunkReader atomChunkReader = new AtomChunkReader(chunk0, dis0);
        AtomChunk atomChunk = atomChunkReader.read();

        DataInputStream dis1 = new DataInputStream(new ByteArrayInputStream(bytes));
        Chunk chunk1 = new Chunk(ChunkId.EXPT, 296, 40);
        dis1.skipBytes(chunk1.offset());
        ExportTableChunkReader exportTableChunkReader = new ExportTableChunkReader(chunk1, dis1, atomChunk);
        ExportTableChunk exportTableChunk = exportTableChunkReader.read();
        assertNotNull(exportTableChunk);
        assertNotNull(exportTableChunk.exports());
        List exports = exportTableChunk.exports();
        assertEquals(mfa("pid", "loop", 0, 2), exports.head());
        exports = exports.tail();
        assertEquals(mfa("pid", "module_info", 0, 7), exports.head());
        exports = exports.tail();
        assertEquals(mfa("pid", "module_info", 1, 9), exports.head());
        assertEquals(List.nil, exports.tail());
    }

    private static FunctionSignature mfa(String m, String f, int a, int l) {
        return new FunctionSignature(Atom.of(m), Atom.of(f), Integer.of(a), Integer.of(l));
    }

}
