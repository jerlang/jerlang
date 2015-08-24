package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Float;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class FloatTest extends AbstractOpTest {

    public FloatTest() {
        super("float.beam");
    }

    @Test
    public void test_float() throws ThrowException {
        Float expected = new Float(-4.0);
        List params = List.of(Float.of(1.0));
        Term result = Erlang.apply(Atom.of("float"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
