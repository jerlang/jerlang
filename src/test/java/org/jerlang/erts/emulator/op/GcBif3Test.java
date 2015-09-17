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

public class GcBif3Test extends AbstractOpTest {

    public GcBif3Test() {
        super("gc_bif3.beam");
    }

    @Test
    public void test_gc_bif3() throws ThrowException {
        Binary expected = new Binary(new int[] { 2, 3, 4 });
        Binary binary = new Binary(new int[] { 1, 2, 3, 4, 5 });
        List params1 = List.of(binary, Integer.of(1), Integer.of(3));
        Term result1 = Erlang.apply(Atom.of("gc_bif3"), Atom.of("test"), params1);
        assertEquals(expected, result1);
    }

    @Test
    public void test_gc_bif3_2() throws ThrowException {
        Binary expected = new Binary(new int[] { 2, 3, 4 });
        Binary binary = new Binary(new int[] { 1, 2, 3, 4, 5 });
        List params1 = List.of(binary, Integer.of(4), Integer.of(-3));
        Term result1 = Erlang.apply(Atom.of("gc_bif3"), Atom.of("test"), params1);
        assertEquals(expected, result1);
    }

}
