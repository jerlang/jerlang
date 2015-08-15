package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TryCatchTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/trycatch.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_trycatch() throws ThrowException {
        Atom expected = Atom.of("exception");
        Term result = Erlang.apply(Atom.of("trycatch"), Atom.of("test"), List.nil);
        assertEquals(expected, result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "trycatch.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
