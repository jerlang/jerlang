package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class MapsTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/maptest.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_float() throws ThrowException {
        Integer expected = new Integer(4);
        Term result = Erlang.apply(Atom.of("maptest"), Atom.of("test"), List.nil);
        assertEquals(expected, result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "maptest.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
