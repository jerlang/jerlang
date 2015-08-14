package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class FloatTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/float.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_float() {
        Float expected = new Float(-4.0);
        List params = List.of(Float.of(1.0));
        Term result = Erlang.apply(Atom.of("float"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "float.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
