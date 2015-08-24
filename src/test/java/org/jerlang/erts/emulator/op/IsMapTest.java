package org.jerlang.erts.emulator.op;

import static org.junit.Assert.assertEquals;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.Boolean;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;
import org.junit.Test;

public class IsMapTest extends AbstractOpTest {

    private List params1 = List.of(Atom.of("map"));
    private List params2 = List.of(new Map());

    public IsMapTest() {
        super("is_map.beam");
    }

    @Test
    public void test1() throws ThrowException {
        Term result1 = Erlang.apply(Atom.of("is_map"), Atom.of("is_map1"), params1);
        assertEquals(Boolean.am_false, result1);

        Term result2 = Erlang.apply(Atom.of("is_map"), Atom.of("is_map1"), params2);
        assertEquals(Boolean.am_true, result2);
    }

    @Test
    public void test2() throws ThrowException {
        Term result1 = Erlang.apply(Atom.of("is_map"), Atom.of("is_map2"), params1);
        assertEquals(Boolean.am_false, result1);

        Term result2 = Erlang.apply(Atom.of("is_map"), Atom.of("is_map2"), params2);
        assertEquals(Boolean.am_true, result2);
    }

}
