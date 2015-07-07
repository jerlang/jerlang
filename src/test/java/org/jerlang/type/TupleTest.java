package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class TupleTest {

    @Test
    public void testEmptyTuple() {
        assertEquals(0, Tuple.of().arity());
    }

    @Test
    public void testTuple1() {
        assertEquals(1, Tuple.of(Atom.of("a")).arity());
    }

    @Test
    public void testTuple2() {
        assertEquals(2, Tuple.of(Atom.of("a"), Atom.of("b")).arity());
    }

}
