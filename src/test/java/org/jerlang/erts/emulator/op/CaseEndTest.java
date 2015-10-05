package org.jerlang.erts.emulator.op;

import static org.jerlang.type.List.nil;
import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Term;
import org.junit.Test;

public class CaseEndTest extends AbstractOpTest {

    private static final Atom case_end = Atom.of("case_end");
    private static final Atom error = Atom.of("error");
    private static final Atom test = Atom.of("test");

    public CaseEndTest() {
        super("case_end.beam");
    }

    @Test
    public void test_case_end() throws ThrowException {
        Term result = Erlang.apply(case_end, test, nil);
        assertEquals(error, result);
    }

}
