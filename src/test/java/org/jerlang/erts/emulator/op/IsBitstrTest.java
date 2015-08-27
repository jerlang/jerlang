package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.BitString;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class IsBitstrTest extends AbstractOpTest {

    private static final Atom is_bitstr = Atom.of("is_bitstr");
    private static final Atom test = Atom.of("test");
    private static final Atom test2 = Atom.of("test2");

    public IsBitstrTest() {
        super("is_bitstr.beam");
    }

    @Test
    public void test1() throws Error {
        List params1 = List.of(List.nil);
        Term result1 = Erlang.apply(is_bitstr, test, params1);
        assertEquals(Boolean.am_false, result1);

        List params2 = List.of(BitString.of(1, 2, 3));
        Term result2 = Erlang.apply(is_bitstr, test, params2);
        assertEquals(Boolean.am_true, result2);
    }

    @Test
    public void test2() throws Error {
        List params1 = List.of(List.nil);
        Term result1 = Erlang.apply(is_bitstr, test2, params1);
        assertEquals(Boolean.am_false, result1);

        List params2 = List.of(BitString.of(1, 2, 3));
        Term result2 = Erlang.apply(is_bitstr, test2, params2);
        assertEquals(Boolean.am_true, result2);
    }

}
