package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsRestore2Test extends AbstractOpTest {

    public BsRestore2Test() {
        super("bs_restore2.beam");
    }

    @Test
    public void test_bs_restore2_1() throws ThrowException {
        Atom expected = Atom.of("zero");
        List params = List.of(new Binary(new int[] { 0 }));
        Term result = Erlang.apply(Atom.of("bs_restore2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @Test
    public void test_bs_restore2_2() throws ThrowException {
        Atom expected = Atom.of("halfbyte");
        List params = List.of(new Binary(new int[] { 8 }));
        Term result = Erlang.apply(Atom.of("bs_restore2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @Test
    public void test_bs_restore2_3() throws ThrowException {
        Atom expected = Atom.of("fullbyte");
        List params = List.of(new Binary(new int[] { 255 }));
        Term result = Erlang.apply(Atom.of("bs_restore2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
