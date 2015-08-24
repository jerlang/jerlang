package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class TryCatchTest extends AbstractOpTest {

    public TryCatchTest() {
        super("trycatch.beam");
    }

    @Test
    public void test_trycatch() throws ThrowException {
        Atom expected = Atom.of("exception");
        Term result = Erlang.apply(Atom.of("trycatch"), Atom.of("test"), List.nil);
        assertEquals(expected, result);
    }

}
