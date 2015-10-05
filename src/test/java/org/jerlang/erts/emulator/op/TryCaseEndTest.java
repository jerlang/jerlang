package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class TryCaseEndTest extends AbstractOpTest {

    public TryCaseEndTest() {
        super("try_case_end.beam");
    }

    @Test
    public void test_trycatch() throws ThrowException {
        Atom expected = Atom.of("ok");
        Term result = Erlang.apply(Atom.of("try_case_end"), Atom.of("test"), List.nil);
        assertEquals(expected, result);
    }

}
