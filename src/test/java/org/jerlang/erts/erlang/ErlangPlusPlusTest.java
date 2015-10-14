package org.jerlang.erts.erlang;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.emulator.op.AbstractOpTest;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class ErlangPlusPlusTest extends AbstractOpTest {

    public ErlangPlusPlusTest() {
        super("plusplus.beam");
    }

    @Test
    public void testPlusPlus() throws ThrowException {
        Integer zero = Integer.ZERO;
        Integer one = Integer.ONE;
        Integer two = Integer.of(2);
        List expected = List.of(one, two, zero);
        List params = List.of(List.of(one, two));
        Term result = Erlang.apply(Atom.of("plusplus"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
