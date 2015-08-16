package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class BsGetBinary2Test {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/bs_get_binary2.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_bs_float() throws ThrowException {
        Binary binary = new Binary(new int[] { 17, 2, 160 }, 4);
        Tuple expected = Tuple.of(Integer.of(1), binary);
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bs_get_binary2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "bs_get_binary2.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
