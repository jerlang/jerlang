package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsMatchStringTest extends AbstractOpTest {

    public BsMatchStringTest() {
        super("bs_match_string.beam");
    }

    @Test
    public void test_bs_match_string1() throws ThrowException {
        Atom expected = Atom.of("error");
        List params = List.of(new Binary(new int[] { 18, 1, 2 }));
        Term result = Erlang.apply(Atom.of("bs_match_string"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

    @Test
    public void test_bs_match_string2() throws ThrowException {
        Binary expected = new Binary(new int[] { 1, 2 });
        List params = List.of(new Binary(new int[] { 16, 1, 2 }));
        Term result = Erlang.apply(Atom.of("bs_match_string"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
