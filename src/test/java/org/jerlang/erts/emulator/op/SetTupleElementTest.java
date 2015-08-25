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

public class SetTupleElementTest extends AbstractOpTest {

    public SetTupleElementTest() {
        super("set_tuple_element.beam");
    }

    @Test
    public void test_set_tuple_element() throws ThrowException {
        List params = List.of(Tuple.of(Integer.of(1), Integer.of(2)));
        Tuple expected = Tuple.of(Integer.of(0), Integer.of(2), Integer.of(0), Integer.of(4));
        Term result = Erlang.apply(Atom.of("set_tuple_element"), Atom.of("test"), params);
        assertEquals(expected, result);
    }

}
