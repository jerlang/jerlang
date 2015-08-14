package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TupleTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/tuple.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_tuple() {
        Integer one = Integer.of(1);
        Tuple expected = Tuple.of(one, one);
        List params = List.of(Tuple.of(one, List.nil));
        Term result = Erlang.apply(Atom.of("tuple"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "tuple.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
