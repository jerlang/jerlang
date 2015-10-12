package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.op.AbstractOpTest;
import org.jerlang.exception.ThrowException;
import org.junit.Test;

public class BigNumTest extends AbstractOpTest {

    public BigNumTest() {
        super("bignum.beam");
    }

    @Test
    public void test_bs_put_string() throws ThrowException {
        Integer expected = new Integer(
            new BigInteger("115792089237316195423570985008687907853269984665640564039457584007913129639936"));
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bignum"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
