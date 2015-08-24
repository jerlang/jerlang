package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Term;
import org.junit.Test;

public class ListLengthTest extends AbstractOpTest {

    public ListLengthTest() {
        super("example4.beam");
    }

    @Test
    public void test_list_length() throws ThrowException {
        Atom a = Atom.of("a");
        List params = List.of(List.of(a, a, a, a, a, a, a, a, a, a));
        Term result = Erlang.apply(Atom.of("example4"), Atom.of("list_length"), params);
        assertEquals(Integer.of(10), result);
    }

}
