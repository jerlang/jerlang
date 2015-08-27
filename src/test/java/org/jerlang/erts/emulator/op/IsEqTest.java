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

public class IsEqTest extends AbstractOpTest {

    public IsEqTest() {
        super("is_eq.beam");
    }

    @Test
    public void test_is_eq() throws ThrowException {
        List params1 = List.of(Integer.of(0));
        Term result1 = Erlang.apply(Atom.of("is_eq"), Atom.of("eq"), params1);
        assertEquals(Boolean.am_true, result1);

        List params2 = List.of(Integer.of(1));
        Term result2 = Erlang.apply(Atom.of("is_eq"), Atom.of("eq"), params2);
        assertEquals(Boolean.am_false, result2);
    }

    @Test
    public void test_is_ne() throws ThrowException {
        List params1 = List.of(Integer.of(1));
        Term result1 = Erlang.apply(Atom.of("is_eq"), Atom.of("ne"), params1);
        assertEquals(Boolean.am_true, result1);

        List params2 = List.of(Integer.of(0));
        Term result2 = Erlang.apply(Atom.of("is_eq"), Atom.of("ne"), params2);
        assertEquals(Boolean.am_false, result2);
    }

}
