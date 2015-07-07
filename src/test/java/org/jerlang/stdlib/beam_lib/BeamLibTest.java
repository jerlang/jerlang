package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class BeamLibTest {

    @Test
    public void testInfo1() throws IOException {
        File tempFile = File.createTempFile("junit", "temp");
        String tempFilename = tempFile.getCanonicalPath();
        Term result = BeamLib.info(tempFilename);
        assertTrue(result instanceof Tuple);
        Tuple resultTuple = (Tuple) result;
        Tuple expectedTuple = Tuple.of(
            Atom.of("error"),
            Atom.of("beam_lib"),
            Tuple.of(
                Atom.of("invalid_beam_file"),
                Term.of(tempFilename),
                Integer.of(0)
                ));
        assertEquals(expectedTuple, resultTuple);
    }

    @Test
    public void testInfo2() throws IOException {
        File file = new File("src/test/resources/pid.beam");
        String filename = file.getCanonicalPath();
        Term result = BeamLib.info(filename);
        assertTrue(result instanceof List);
        List resultList = (List) result;
        Tuple filenameTuple = Tuple.of(Atom.of("file"), Term.of(filename));
        assertEquals(filenameTuple, resultList.head());
        resultList = resultList.tail();
        assertTrue(resultList.head() instanceof Tuple);
        Tuple chunksTuple = (Tuple) resultList.head();
        assertEquals(2, chunksTuple.arity());
        assertEquals(Atom.of("chunks"), chunksTuple.element(0));
        assertTrue(chunksTuple.element(1) instanceof List);
        List chunkList = (List) chunksTuple.element(1);

        assertChunk(chunkList.head(), "Atom", 20, 75);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "Code", 104, 116);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "StrT", 228, 0);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "ImpT", 236, 52);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "ExpT", 296, 40);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "LitT", 344, 33);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "LocT", 388, 4);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "Attr", 400, 40);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "CInf", 448, 115);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "Abst", 572, 0);
        chunkList = chunkList.tail();
        assertChunk(chunkList.head(), "Line", 580, 23);
        chunkList = chunkList.tail();

        assertEquals(List.nil, chunkList);
    }

    private void assertChunk(Term term, String id, int offset, int length) {
        assertTrue(term instanceof Tuple);
        assertEquals(Term.of(id), ((Tuple) term).element(0));
        assertEquals(Integer.of(offset), ((Tuple) term).element(1));
        assertEquals(Integer.of(length), ((Tuple) term).element(2));
    }

}
