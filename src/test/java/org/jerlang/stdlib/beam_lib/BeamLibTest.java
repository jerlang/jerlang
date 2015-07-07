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
    }

}
