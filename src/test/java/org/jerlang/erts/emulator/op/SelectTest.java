package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SelectTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/select.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_select1() throws ThrowException {
        List params = List.of(Integer.of(2));
        Term result = Erlang.apply(Atom.of("select"), Atom.of("test"), params);
        assertEquals(Str.of("two"), result);
    }

    @Test
    public void test_select2() throws ThrowException {
        List params = List.of(Str.of("two"));
        Term result = Erlang.apply(Atom.of("select"), Atom.of("test"), params);
        assertEquals(Integer.of(2), result);
    }

    @Test
    public void test_select3() throws ThrowException {
        List params = List.of(Tuple.of(Atom.of("a"), Atom.of("b")));
        Term result = Erlang.apply(Atom.of("select"), Atom.of("test"), params);
        assertEquals(Integer.of(2), result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "select.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
