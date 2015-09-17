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

public class BsUtfSizeTest extends AbstractOpTest {

    private static final Atom ok = Atom.of("ok");

    public BsUtfSizeTest() {
        super("bs_utf_size.beam");
    }

    @Test
    public void test_bs_utf8_size() throws ThrowException {
        List params = List.of(Integer.of('A'), new Binary(new int[] { 'A' }));
        Term result = Erlang.apply(Atom.of("bs_utf_size"), Atom.of("test8"), params);
        assertEquals(ok, result);
    }

    @Test
    public void test_bs_utf16_size() throws ThrowException {
        List params = List.of(Integer.of('A'), new Binary(new int[] { 0, 'A' }));
        Term result = Erlang.apply(Atom.of("bs_utf_size"), Atom.of("test16"), params);
        assertEquals(ok, result);
    }

    @Test
    public void test_bs_put_utf32() throws ThrowException {
        List params = List.of(Integer.of('A'), new Binary(new int[] { 0, 0, 0, 'A' }));
        Term result = Erlang.apply(Atom.of("bs_utf_size"), Atom.of("test32"), params);
        assertEquals(ok, result);
    }

}
