package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class TupleTest extends AbstractOpTest {

    public TupleTest() {
        super("tuple.beam");
    }

    @Test
    public void test_tuple() throws ThrowException {
        Integer one = Integer.of(1);
        Tuple expected = Tuple.of(one, one);
        List params = List.of(Tuple.of(one, List.nil));
        Term result = Erlang.apply(Atom.of("tuple"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
