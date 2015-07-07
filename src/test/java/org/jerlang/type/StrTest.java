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
        assertEquals("[65, 66, 67]", Term.of("ABC").toString());
    }

}
