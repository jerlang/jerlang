package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class ApplyTest extends AbstractOpTest {

    private static final Atom apply = Atom.of("apply");
    private static final Atom module = Atom.of("module");
    private static final Atom test = Atom.of("test");

    public ApplyTest() {
        super("apply.beam");
    }

    @Test
    public void test_apply() throws ThrowException {
        List params = List.of(apply);
        Term result = Erlang.apply(apply, test, params);
        Term expected = Tuple.of(module, apply);
        assertEquals(expected, result);
    }

}
