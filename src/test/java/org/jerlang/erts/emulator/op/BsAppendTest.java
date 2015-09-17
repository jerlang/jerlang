package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsAppendTest extends AbstractOpTest {

    public BsAppendTest() {
        super("bs_append.beam");
    }

    @Test
    public void test_bs_append() throws ThrowException {
        Binary expected = new Binary(new int[] { 1, 2, 3, 0 });
        List params = List.of(new Binary(new int[] { 1, 2, 3 }));
        Term result = Erlang.apply(Atom.of("bs_append"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
