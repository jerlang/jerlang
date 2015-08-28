package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class ApplyLastTest extends AbstractOpTest {

    private static final Atom apply_last = Atom.of("apply_last");
    private static final Atom test = Atom.of("test");

    public ApplyLastTest() {
        super("apply_last.beam");
    }

    @Test
    public void test_apply() throws ThrowException {
        List params = List.of(apply_last);
        Term result = Erlang.apply(apply_last, test, params);
        assertEquals(apply_last, result);
    }

}
