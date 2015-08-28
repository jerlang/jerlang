package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class CallFunTest extends AbstractOpTest {

    public CallFunTest() {
        super("call_fun.beam");
    }

    @Test
    public void test_call_fun() throws ThrowException {
        List params = List.of(Integer.of(7));
        Term result = Erlang.apply(Atom.of("call_fun"), Atom.of("test"), params);
        assertEquals(Integer.of(7 + 7), result);
    }

}
