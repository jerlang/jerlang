package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Binary;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsPutFloatTest extends AbstractOpTest {

    public BsPutFloatTest() {
        super("bs_put_float.beam");
    }

    @Test
    public void test_bs_put_float() throws ThrowException {
        Binary expected = new Binary(new int[] { 63, 248, 0, 0, 0, 0, 0, 0 });
        List params = List.of(new Float(1.5));
        Term result = Erlang.apply(Atom.of("bs_put_float"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
