package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class CallLastTest extends AbstractOpTest {

    public CallLastTest() {
        super("call_last.beam");
    }

    @Test
    public void test_call_fun() throws ThrowException {
        List params = List.of(Integer.of(10));
        Term result = Erlang.apply(Atom.of("call_last"), Atom.of("test"), params);
        assertEquals(Integer.of(31), result);
    }

}
