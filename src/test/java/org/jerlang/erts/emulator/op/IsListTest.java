package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class IsListTest extends AbstractOpTest {

    public IsListTest() {
        super("test_functions.beam");
    }

    @Test
    public void test() throws ThrowException {
        List params1 = List.of(Integer.of(1));
        Term result1 = Erlang.apply(Atom.of("test_functions"), Atom.of("test_is_list"), params1);
        assertEquals(Boolean.am_false, result1);
        List params2 = List.of(List.nil);
        Term result2 = Erlang.apply(Atom.of("test_functions"), Atom.of("test_is_list"), params2);
        assertEquals(Boolean.am_true, result2);
    }

}
