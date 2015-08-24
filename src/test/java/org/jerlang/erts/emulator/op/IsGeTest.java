package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class IsGeTest extends AbstractOpTest {

    public IsGeTest() {
        super("example1.beam");
    }

    @Test
    public void test_is_ge() throws ThrowException {
        // Positive
        List params1 = List.of(Integer.of(1));
        Term result1 = Erlang.apply(Atom.of("example1"), Atom.of("test"), params1);
        assertEquals(Atom.of("positive"), result1);
        // Negative
        List params2 = List.of(Integer.of(-1));
        Term result2 = Erlang.apply(Atom.of("example1"), Atom.of("test"), params2);
        assertEquals(Atom.of("negative"), result2);
    }

}
