package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class BeamLibTest {

    @Test
    public void testInfo() throws IOException {
        File tempFile = File.createTempFile("junit", "temp");
        String tempFilename = tempFile.getCanonicalPath();
        Term result = BeamLib.info(tempFilename);
        assertTrue(result instanceof List);
        List resultList = (List) result;
        Tuple filenameTuple = Tuple.of(Atom.of("file"), Term.of(tempFilename));
        assertEquals(filenameTuple, resultList.head());
    }

}
