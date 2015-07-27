package org.jerlang.stdlib.beam_lib;

import static org.junit.Assert.assertEquals;

import org.jerlang.stdlib.BeamLib;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class BeamLibMD5Test {

    @Test
    public void testMD5() {
        Term result = BeamLib.md5(Str.of("src/test/resources/pid.beam"));
        Term expected = Tuple.of(
            Atom.of("ok"),
            Tuple.of(
                Atom.of("pid"),
                Binary.of(
                    166, 215, 253, 173, 96, 127, 227, 128,
                    199, 250, 156, 228, 96, 203, 0, 62)
                ));
        assertEquals(expected, result);
    }

}
