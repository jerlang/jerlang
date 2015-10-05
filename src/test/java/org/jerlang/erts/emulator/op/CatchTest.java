package org.jerlang.erts.emulator.op;

import static org.jerlang.type.List.nil;
import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Term;
import org.junit.Test;

public class CatchTest extends AbstractOpTest {

    private static final Atom test_catch = Atom.of("test_catch");
    private static final Atom ok = Atom.of("ok");
    private static final Atom test = Atom.of("test");

    public CatchTest() {
        super("test_catch.beam");
    }

    @Test
    public void test_catch() throws ThrowException {
        Term result = Erlang.apply(test_catch, test, nil);
        assertEquals(ok, result);
    }

}
