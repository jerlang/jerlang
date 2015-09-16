package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BsPutStringTest extends AbstractOpTest {

    public BsPutStringTest() {
        super("bs_put_string.beam");
    }

    @Test
    public void test_bs_put_string() throws ThrowException {
        Atom expected = Atom.of("ok");
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bs_put_string"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
