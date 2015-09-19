package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsSave2Test extends AbstractOpTest {

    public BsSave2Test() {
        super("bs_save2.beam");
    }

    @Test
    public void test_bs_save2_1() throws ThrowException {
        Atom expected = Atom.of("a");
        List params = List.of(new Binary(new int[] { 0 }));
        Term result = Erlang.apply(Atom.of("bs_save2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @Test
    public void test_bs_save2_2() throws ThrowException {
        Atom expected = Atom.of("b");
        List params = List.of(new Binary(new int[] { 1, 2, 3 }));
        Term result = Erlang.apply(Atom.of("bs_save2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @Test
    public void test_bs_save2_3() throws ThrowException {
        Atom expected = Atom.of("c");
        List params = List.of(new Binary(new int[] { 255 }));
        Term result = Erlang.apply(Atom.of("bs_save2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
