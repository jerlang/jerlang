package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ListTest {

    @Test
    public void testListOf() {
        List list = List.of(Str.of("a"), Str.of("b"), Str.of("c"));
        assertEquals("[\"a\",\"b\",\"c\"]", list.toString());
    }

}
