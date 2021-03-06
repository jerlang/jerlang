package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.junit.Test;

public class IsTupleTest extends AbstractOpTest {

    public IsTupleTest() {
        super("test_functions.beam");
    }

    @Test
    public void test() throws ThrowException {
        List params1 = List.of(List.nil);
        Term result1 = Erlang.apply(Atom.of("test_functions"), Atom.of("test_is_tuple"), params1);
        assertEquals(Boolean.am_false, result1);
        List params2 = List.of(Tuple.of());
        Term result2 = Erlang.apply(Atom.of("test_functions"), Atom.of("test_is_tuple"), params2);
        assertEquals(Boolean.am_true, result2);
    }

}
