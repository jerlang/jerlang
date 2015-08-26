package org.jerlang.erts.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.op.AbstractOpTest;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Reference;
import org.jerlang.type.Term;
import org.junit.Test;

public class ErlangMakeRefTest extends AbstractOpTest {

    public ErlangMakeRefTest() {
        super("is_ref.beam");
    }

    @Test
    public void test_ref() throws ThrowException {
        Term ref = Erlang.apply(Atom.of("is_ref"), Atom.of("test_get_ref"), List.nil);
        assertTrue(ref instanceof Reference);
        assertEquals(Boolean.am_true, Erlang.apply(Atom.of("is_ref"), Atom.of("test_is_ref"), List.of(ref)));
        assertEquals(Boolean.am_false, Erlang.apply(Atom.of("is_ref"), Atom.of("test_is_ref"), List.of(List.nil)));
    }

}
