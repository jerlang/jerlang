package org.jerlang.erts.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Reference;
import org.jerlang.type.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ErlangMakeRefTest {

    @Before
    public void prepare() throws Exception {
        String[] cmd = { "cp", "src/test/resources/is_ref.beam", "." };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

    @Test
    public void test_ref() throws ThrowException {
        Term ref = Erlang.apply(Atom.of("is_ref"), Atom.of("test_get_ref"), List.nil);
        assertTrue(ref instanceof Reference);
        assertEquals(Boolean.am_true, Erlang.apply(Atom.of("is_ref"), Atom.of("test_is_ref"), List.of(ref)));
        assertEquals(Boolean.am_false, Erlang.apply(Atom.of("is_ref"), Atom.of("test_is_ref"), List.of(List.nil)));
    }

    @After
    public void cleanup() throws Exception {
        String[] cmd = { "rm", "is_ref.beam" };
        Runtime.getRuntime().exec(cmd).waitFor();
    }

}
