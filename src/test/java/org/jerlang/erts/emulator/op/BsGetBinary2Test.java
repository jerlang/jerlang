package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.BitString;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class BsGetBinary2Test extends AbstractOpTest {

    public BsGetBinary2Test() {
        super("bs_get_binary2.beam");
    }

    @Test
    public void test_bs_float() throws ThrowException {
        BitString bitString = new BitString(new int[] { 17, 2, 160 }, 4);
        Tuple expected = Tuple.of(Integer.of(1), bitString);
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bs_get_binary2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
