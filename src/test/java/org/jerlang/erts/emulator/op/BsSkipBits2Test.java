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

public class BsSkipBits2Test extends AbstractOpTest {

    public BsSkipBits2Test() {
        super("bs_skip_bits2.beam");
    }

    @Test
    public void test_bs_skip_bits2() throws ThrowException {
        Integer expected = Integer.of('B');
        List params = List.of(new Binary(new int[] { 'A', 'B' }));
        Term result = Erlang.apply(Atom.of("bs_skip_bits2"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
