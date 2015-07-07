package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class StrTest {

    @Test
    public void testToString1() {
        assertEquals("[]", Term.of("").toString());
    }

    @Test
    public void testToString2() {
        assertEquals("ABC", Term.of("ABC").toString());
    }

}
