package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class IsNeExactTest extends AbstractOpTest {

    public IsNeExactTest() {
        super("is_ne_exact.beam");
    }

    @Test
    public void test_is_ge() throws ThrowException {
        List params1 = List.of(Integer.of(1));
        Term result1 = Erlang.apply(Atom.of("is_ne_exact"), Atom.of("test"), params1);
        assertEquals(Boolean.am_true, result1);

        List params2 = List.of(Integer.of(-1));
        Term result2 = Erlang.apply(Atom.of("is_ne_exact"), Atom.of("test"), params2);
        assertEquals(Boolean.am_true, result2);
    }

}
