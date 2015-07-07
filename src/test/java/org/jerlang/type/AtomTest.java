package org.jerlang.type;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class AtomTest {

    @Test
    public void testToString() {
        assertEquals("atom", new Atom("atom").toString());
    }

}
