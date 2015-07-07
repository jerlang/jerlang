package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ChunkIdentifierTest {

    @Test
    public void testAbst() {
        assertEquals("Abst", ChunkIdentifier.ABST.toString());
        assertEquals(0x41627374, ChunkIdentifier.ABST.toInt());
    }

    @Test
    public void testAtom() {
        assertEquals("Atom", ChunkIdentifier.ATOM.toString());
        assertEquals(0x41746f6d, ChunkIdentifier.ATOM.toInt());
    }

    @Test
    public void testAttr() {
        assertEquals("Attr", ChunkIdentifier.ATTR.toString());
        assertEquals(0x41747472, ChunkIdentifier.ATTR.toInt());
    }

    @Test
    public void testCInf() {
        assertEquals("CInf", ChunkIdentifier.CINF.toString());
        assertEquals(0x43496e66, ChunkIdentifier.CINF.toInt());
    }

    @Test
    public void testCode() {
        assertEquals("Code", ChunkIdentifier.CODE.toString());
        assertEquals(0x436f6465, ChunkIdentifier.CODE.toInt());
    }

    @Test
    public void testExpT() {
        assertEquals("ExpT", ChunkIdentifier.EXPT.toString());
        assertEquals(0x45787054, ChunkIdentifier.EXPT.toInt());
    }

    @Test
    public void testFunT() {
        assertEquals("FunT", ChunkIdentifier.FUNT.toString());
        assertEquals(0x46756e54, ChunkIdentifier.FUNT.toInt());
    }

    @Test
    public void testImpT() {
        assertEquals("ImpT", ChunkIdentifier.IMPT.toString());
        assertEquals(0x496d7054, ChunkIdentifier.IMPT.toInt());
    }

    @Test
    public void testLine() {
        assertEquals("Line", ChunkIdentifier.LINE.toString());
        assertEquals(0x4c696e65, ChunkIdentifier.LINE.toInt());
    }

    @Test
    public void testLitT() {
        assertEquals("LitT", ChunkIdentifier.LITT.toString());
        assertEquals(0x4c697454, ChunkIdentifier.LITT.toInt());
    }

    @Test
    public void testLocT() {
        assertEquals("LocT", ChunkIdentifier.LOCT.toString());
        assertEquals(0x4c6f6354, ChunkIdentifier.LOCT.toInt());
    }

    @Test
    public void testStrT() {
        assertEquals("StrT", ChunkIdentifier.STRT.toString());
        assertEquals(0x53747254, ChunkIdentifier.STRT.toInt());
    }

}
