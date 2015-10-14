package org.jerlang.erts.erlang;

import static org.jerlang.erts.Erlang.apply;
import static org.jerlang.type.Boolean.am_false;
import static org.jerlang.type.Boolean.am_true;
import static org.jerlang.type.List.nil;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.jerlang.erts.emulator.op.AbstractOpTest;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Reference;
import org.jerlang.type.Term;
import org.junit.Test;

public class ErlangMakeRefTest extends AbstractOpTest {

    private static final Atom is_ref = Atom.of("is_ref");
    private static final Atom test_get_ref = Atom.of("test_get_ref");
    private static final Atom test_is_ref = Atom.of("test_is_ref");

    public ErlangMakeRefTest() {
        super("is_ref.beam");
    }

    @Test
    public void test_ref() throws ThrowException {
        Term ref = apply(is_ref, test_get_ref, nil);
        assertTrue(ref instanceof Reference);
        assertEquals(am_true, apply(is_ref, test_is_ref, List.of(ref)));
        assertEquals(am_false, apply(is_ref, test_is_ref, List.of(nil)));
    }

}
