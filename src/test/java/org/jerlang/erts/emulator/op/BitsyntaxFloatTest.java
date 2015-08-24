package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class BitsyntaxFloatTest extends AbstractOpTest {

    public BitsyntaxFloatTest() {
        super("bs_float.beam");
    }

    @Test
    public void test_bs_float() throws ThrowException {
        Float expected = new Float(1.23);
        List params = List.nil;
        Term result = Erlang.apply(Atom.of("bs_float"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
