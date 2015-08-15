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

public class Bitsyntax2Test {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/bitsyntax2.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_bitsyntax1() throws ThrowException {
        Integer one = Integer.of(1);
        Integer two = Integer.of(2);
        Integer three = Integer.of(3);
        List expected = List.of(one, one, two, two, three, three);
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bitsyntax2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "bitsyntax2.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
