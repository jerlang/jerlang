package org.jerlang.stdlib.lists;

import static org.junit.Assert.assertEquals;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.junit.Test;

public class ListsAppendTest {

    @Test
    public void testAppend1() {
        Atom a = Atom.of("a");
        Atom b = Atom.of("b");
        Atom c = Atom.of("c");
        Atom d = Atom.of("d");
        List list1 = List.of(a, b);
        List list2 = List.of(c, d);
        List given = List.of(list1, list2);
        List expected = List.of(a, b, c, d);
        assertEquals(expected, Lists.append(given));
    }

    @Test
    public void testAppend2() {
        Atom a = Atom.of("a");
        Atom b = Atom.of("b");
        Atom c = Atom.of("c");
        Atom d = Atom.of("d");
        List given1 = List.of(a, b);
        List given2 = List.of(c, d);
        List expected = List.of(a, b, c, d);
        assertEquals(expected, Lists.append(given1, given2));
    }

}
