package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.op.AbstractOpTest;
import org.junit.Test;

public class BitStringTest extends AbstractOpTest {

    public BitStringTest() {
        super("bitstring.beam");
    }

    @Test
    public void testToString1() throws Error {
        BitString bs = new BitString(new int[] { 1, 2, 3 });
        List params = List.of(bs);
        Term result = Erlang.apply(Atom.of("bitstring"), Atom.of("test"), params);
        Tuple expected = Tuple.of(bs, Integer.of(3));
        assertEquals(expected, result);
    }

}
