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

public class RecursionTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/example2.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_fac() throws ThrowException {
        List params = List.of(Integer.of(4));
        Term result = Erlang.apply(Atom.of("example2"), Atom.of("fac"), params);
        assertEquals(Integer.of(4 * 3 * 2 * 1), result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "example3.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
