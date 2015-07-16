package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ChunkIdTest {

    @Test
    public void testAbst() {
        assertEquals("Abst", ChunkId.ABST.toString());
        assertEquals(0x41627374, ChunkId.ABST.toInt());
    }

    @Test
    public void testAtom() {
        assertEquals("Atom", ChunkId.ATOM.toString());
        assertEquals(0x41746f6d, ChunkId.ATOM.toInt());
    }

    @Test
    public void testAttr() {
        assertEquals("Attr", ChunkId.ATTR.toString());
        assertEquals(0x41747472, ChunkId.ATTR.toInt());
    }

    @Test
    public void testCInf() {
        assertEquals("CInf", ChunkId.CINF.toString());
        assertEquals(0x43496e66, ChunkId.CINF.toInt());
    }

    @Test
    public void testCode() {
        assertEquals("Code", ChunkId.CODE.toString());
        assertEquals(0x436f6465, ChunkId.CODE.toInt());
    }

    @Test
    public void testExpT() {
        assertEquals("ExpT", ChunkId.EXPT.toString());
        assertEquals(0x45787054, ChunkId.EXPT.toInt());
    }

    @Test
    public void testFunT() {
        assertEquals("FunT", ChunkId.FUNT.toString());
        assertEquals(0x46756e54, ChunkId.FUNT.toInt());
    }

    @Test
    public void testImpT() {
        assertEquals("ImpT", ChunkId.IMPT.toString());
        assertEquals(0x496d7054, ChunkId.IMPT.toInt());
    }

    @Test
    public void testLine() {
        assertEquals("Line", ChunkId.LINE.toString());
        assertEquals(0x4c696e65, ChunkId.LINE.toInt());
    }

    @Test
    public void testLitT() {
        assertEquals("LitT", ChunkId.LITT.toString());
        assertEquals(0x4c697454, ChunkId.LITT.toInt());
    }

    @Test
    public void testLocT() {
        assertEquals("LocT", ChunkId.LOCT.toString());
        assertEquals(0x4c6f6354, ChunkId.LOCT.toInt());
    }

    @Test
    public void testStrT() {
        assertEquals("StrT", ChunkId.STRT.toString());
        assertEquals(0x53747254, ChunkId.STRT.toInt());
    }

}
