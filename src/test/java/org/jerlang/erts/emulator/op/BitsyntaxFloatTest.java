package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class BitsyntaxFloatTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/bs_float.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_bs_float() throws ThrowException {
        Float expected = new Float(1.23);
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bs_float"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "bs_float.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
