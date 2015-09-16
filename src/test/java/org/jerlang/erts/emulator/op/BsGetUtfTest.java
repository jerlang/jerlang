package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsGetUtfTest extends AbstractOpTest {

    public BsGetUtfTest() {
        super("bs_get_utf.beam");
    }

    @Test
    public void test_bs_get_utf8() throws ThrowException {
        Integer expected = Integer.of('A');
        List params = List.of(new Binary(new int[] { 'A', 'B' }));
        Term result = Erlang.apply(Atom.of("bs_get_utf"), Atom.of("test8"), params);
        assertEquals(expected, result);
    }

    @Test
    public void test_bs_get_utf16() throws ThrowException {
        Integer expected = Integer.of('A');
        List params = List.of(new Binary(new int[] { 0, 65, 'B' }));
        Term result = Erlang.apply(Atom.of("bs_get_utf"), Atom.of("test16"), params);
        assertEquals(expected, result);
    }

    @Test
    public void test_bs_get_utf32() throws ThrowException {
        Integer expected = Integer.of('A');
        List params = List.of(new Binary(new int[] { 0, 0, 0, 65, 'B' }));
        Term result = Erlang.apply(Atom.of("bs_get_utf"), Atom.of("test32"), params);
        assertEquals(expected, result);
    }

}
