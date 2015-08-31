package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.BitString;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class InitBitsTest extends AbstractOpTest {

    public InitBitsTest() {
        super("init_bits.beam");
    }

    @Test
    public void test_bs_init_bits() throws ThrowException {
        test(1, 2);
        test(2, 3);
        test(3, 4);
        test(4, 5);
        test(5, 6);
        test(6, 7);
        test(7, 0);
        test(8, 1);
        test(9, 2);
        test(10, 3);
    }

    private void test(int input, int output) {
        BitString expected = new BitString(new int[] { (output << 5) & 0xFF }, 8 - 3);
        List params = List.of(Integer.of(input));
        Term result = Erlang.apply(Atom.of("init_bits"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
