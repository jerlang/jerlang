package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class IfEndTest extends AbstractOpTest {

    private static final Atom error = Atom.of("error");
    private static final Atom test_catch = Atom.of("if_end");
    private static final Atom ok = Atom.of("ok");
    private static final Atom test = Atom.of("test");

    public IfEndTest() {
        super("if_end.beam");
    }

    @Test
    public void test_if_end1() throws ThrowException {
        Term result = Erlang.apply(test_catch, test, List.of(Integer.ZERO));
        assertEquals(error, result);
    }

    @Test
    public void test_if_end2() throws ThrowException {
        Term result = Erlang.apply(test_catch, test, List.of(Integer.ONE));
        assertEquals(ok, result);
    }

}
