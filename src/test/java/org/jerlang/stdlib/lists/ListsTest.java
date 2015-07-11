package org.jerlang.stdlib.lists;

import static org.junit.Assert.assertEquals;

import org.jerlang.stdlib.Lists;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.junit.Test;

public class ListsTest {

    @Test
    public void testReverse1() {
        assertEquals(List.nil, Lists.reverse(List.nil));
    }

    @Test
    public void testReverse2() {
        List list = new List(Atom.of("element"));
        assertEquals(list, Lists.reverse(list));
    }

    @Test
    public void testReverse3() {
        List list1 = new List(Atom.of("a"), new List(Atom.of("b")));
        List list2 = new List(Atom.of("b"), new List(Atom.of("a")));
        assertEquals(list2, Lists.reverse(list1));
    }

}
