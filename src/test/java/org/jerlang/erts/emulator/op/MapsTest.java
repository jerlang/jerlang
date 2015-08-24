package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class MapsTest extends AbstractOpTest {

    public MapsTest() {
        super("maptest.beam");
    }

    @Test
    public void test_float() throws ThrowException {
        Integer expected = new Integer(4);
        Term result = Erlang.apply(Atom.of("maptest"), Atom.of("test"), List.nil);
        assertEquals(expected, result);
    }

}
