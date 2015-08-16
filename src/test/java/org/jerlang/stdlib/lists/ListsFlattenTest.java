package org.jerlang.stdlib.lists;

import static org.junit.Assert.assertEquals;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.junit.Test;

public class ListsFlattenTest {

    @Test
    public void testFlatten1() {
        Atom a = Atom.of("a");
        Atom b = Atom.of("b");
        Atom c = Atom.of("c");
        Atom d = Atom.of("d");
        List given = List.of(a, List.of(List.of(b), List.of(c)), List.of(d));
        List expected = List.of(a, b, c, d);
        assertEquals(expected, Lists.flatten(given));
    }

}
