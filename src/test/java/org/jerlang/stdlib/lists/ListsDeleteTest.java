package org.jerlang.stdlib.lists;

import static org.junit.Assert.assertEquals;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.junit.Test;

public class ListsDeleteTest {

    @Test
    public void testDelete() {
        Atom a = Atom.of("a");
        Atom b = Atom.of("b");
        Atom c = Atom.of("c");
        Atom d = Atom.of("d");
        List given = List.of(a, c, b, c, d);
        List expected = List.of(a, b, c, d);
        assertEquals(expected, Lists.delete(c, given));
    }

}
