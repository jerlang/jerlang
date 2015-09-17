package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsPutBinaryTest extends AbstractOpTest {

    public BsPutBinaryTest() {
        super("bs_put_binary.beam");
    }

    @Test
    public void test_bs_put_binary() throws ThrowException {
        Binary expected = new Binary(new int[] { 1, 1, 2, 3, 4 });
        List params = List.of(new Binary(new int[] { 1, 2, 3, 4 }));
        Term result = Erlang.apply(Atom.of("bs_put_binary"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
