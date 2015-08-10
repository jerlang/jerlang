package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class IsNumberTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/test_functions.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test() {
        List params1 = List.of(List.nil);
        Term result1 = Erlang.apply(Atom.of("test_functions"), Atom.of("test_is_number"), params1);
        assertEquals(Boolean.am_false, result1);
        List params2 = List.of(Integer.of(1));
        Term result2 = Erlang.apply(Atom.of("test_functions"), Atom.of("test_is_number"), params2);
        assertEquals(Boolean.am_true, result2);
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "test_functions.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
