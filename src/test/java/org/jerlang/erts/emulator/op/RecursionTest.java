package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class RecursionTest extends AbstractOpTest {

    public RecursionTest() {
        super("example2.beam");
    }

    @Test
    public void test_fac() throws ThrowException {
        List params = List.of(Integer.of(4));
        Term result = Erlang.apply(Atom.of("example2"), Atom.of("fac"), params);
        assertEquals(Integer.of(4 * 3 * 2 * 1), result);
    }

}
